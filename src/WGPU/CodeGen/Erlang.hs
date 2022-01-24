{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module WGPU.CodeGen.Erlang (run) where
import WGPU.CodeGen.Parse
import Data.Map (toList, member)
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.Maybe

run :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO ()
run inFile outErlFile outHrlFile = do
    api@(CApi enums structs funs) <- readApi inFile
    let serialize = sortBy (\a b -> compare (fst a) (fst b)) . toList
    maybe (return ()) (`T.writeFile` "") outErlFile
    maybe (return ()) (`T.writeFile` "") outHrlFile
    let emitErl = maybe T.putStrLn T.appendFile outErlFile
    let emitHrl = maybe T.putStrLn T.appendFile outHrlFile
    -- Emit static start of file
    emitErl "-module(wgpu)."
    emitErl "-compile(export_all)."
    emitErl "-on_load(init/0)."
    emitErl $ "-include_lib(\"" <> maybe "wgpu.hrl" T.pack outHrlFile <> "\").\n"
    -- Emit init function
    emitErl "init() -> ok = erlang:load_nif(\"./wgpu_nif\", 0).\n"
    -- Generate stub functions
    emitErl $ T.unlines $ flip map (serialize funs) $ \(_, CFun name retTy params) ->
        "-spec " <> identFormat name <> "(" <> T.intercalate ", " (map (\(CFunParam name ty) -> varFormat name <> " :: " <> fromMaybe "any()" (cTypeToErl api ty)) params) <> ") -> " <> fromMaybe "any()" (cTypeToErlRet api retTy) <> ".\n" <>
        identFormat name <> "(" <> T.intercalate ", " (map (\(CFunParam name ty) -> varFormat name) params) <> ") -> exit(nif_library_not_loaded).\n\n"

    -- Emit default opaque types
    emitHrl "-opaque ptr(T) :: Type."
    -- TODO: Generate other opaque types
    -- Generate dialyzer types from enums
    emitHrl $ T.unlines $ map (\(_, CEnumW32 name members) -> "-type " <> identFormat name <> "() :: " <> T.intercalate " | " (map (\t -> if isAlpha $ T.head t then t else last (T.splitOn "_" $ identFormat name) <> "_" <> t) $ map (\(CEnumW32Member memName memValue) -> identFormat $ T.tail $ T.dropWhile (/= '_') memName) members)) $ serialize enums
    -- Generate records from structs
    emitHrl $ T.unlines $ map (\(_, CStruct name members) -> "-record(" <> identFormat name <> ", {" <> T.intercalate ", " (map (\(CStructMember t ty) -> identFormat t <> maybe "" (" :: " <>) (cTypeToErl api ty)) members) <> "}).") $ filter (\(_, CStruct name members) -> name /= "<ANONYMOUS>") $ serialize structs

    return ()

identFormat :: T.Text -> T.Text
identFormat = T.pack . go . T.unpack
    where
        go [] = []
        go vs@(x:y:z:rest) | isUpper x && isUpper y && isLower z = toLower x : '_' : toLower y : go (z:rest) -- End of acronym
                           | isLower x && isUpper y = x : '_' : toLower y : go (z:rest)
                           | otherwise = goDefault vs
        go vs = goDefault vs

        goDefault (x:xs) = toLower x : go xs
        goDefault [] = []

varFormat :: T.Text -> T.Text
varFormat text =
    let Just (h,t) = T.uncons text
    in T.cons (toUpper h) t

cTypeToErlRet :: CApi -> CType -> Maybe T.Text
cTypeToErlRet _ CVoid = Just "ok"
cTypeToErlRet api ty = cTypeToErl api ty

cTypeToErl :: CApi -> CType -> Maybe T.Text
cTypeToErl _ (CPtr CChar) = Just "string()"
cTypeToErl api (CPtr ty) = (\t -> "ptr(" <> t <> ")") <$> cTypeToErl api ty
cTypeToErl _ CBool = Just "boolean()"
cTypeToErl _ CChar = Just "char()"
cTypeToErl _ CInt = Just "integer()"
cTypeToErl _ CFloat = Just "float()"
cTypeToErl _ CDouble = Just "float()"
cTypeToErl _ (CDefined _ "uint32_t") = Just "integer()"
cTypeToErl _ (CDefined True name) = Just $ identFormat name <> "()"
cTypeToErl (CApi enums structs _) (CDefined _ name) | name `member` structs = Just $ "#" <> identFormat name <> "{}"
                                                    | name `member` enums = Just $ identFormat name <> "()"
                                                    | otherwise = Nothing -- TODO: Check if it is an opaque WGPU type and return ref to it.
cTypeToErl _ _ = Nothing