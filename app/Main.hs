{-# LANGUAGE LambdaCase #-}
import System.Environment
import Language.C
import Language.C.Syntax.AST
import Language.C.System.GCC
import Data.Maybe
import Text.Pretty.Simple
import WGPU.CodeGen.Erlang

main :: IO ()
main = do
    putStrLn "Generating NIFs from C Header..."
    putStrLn "USAGE: GenNif [input.h] [output.c]"
    [inFile, outFile] <- getArgs
    run inFile Nothing Nothing
    -- Right x <- parseCFile (newGCC "gcc") Nothing [] inFile
    -- let CTranslUnit exDecs _ = x -- () <$ x
    -- let decls = mapMaybe (\case CDecl specs xs _ -> Just (specs, xs); _ -> Nothing) $ mapMaybe (\case CDeclExt x -> Just x; _ -> Nothing) exDecs
    -- -- (CStorageSpec (CTypedef _):specs)
    -- pPrint decls
    -- -- print $ pretty cTransUnit
    -- return ()
