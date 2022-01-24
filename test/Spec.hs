{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import NeatInterpolation ( text )
import Data.Text.Encoding ( encodeUtf8 )
import Text.Pretty.Simple ( pPrint )
import Numeric (showHex)

-- TODO: Move code to src/
import Language.C
import Data.Either (isRight)
import Language.C.Data.Ident
import Control.Monad.Trans.Writer.CPS
import Data.Map (Map)
import Prelude hiding (Enum)
import Language.C.Analysis (SignSpec(Unsigned))

main :: IO ()
main = hspec $ do
    describe "Parse C Basic" $ do
        it "Parses Enum" $ do
            let code = [text|
                typedef enum WGPUAdapterType {
                    WGPUAdapterType_DiscreteGPU = 0x00000000,
                    WGPUAdapterType_IntegratedGPU = 0x00000001,
                    WGPUAdapterType_CPU = 0x00000002,
                    WGPUAdapterType_Unknown = 0x00000003,
                    WGPUAdapterType_Force32 = 0x7FFFFFFF
                } WGPUAdapterType;
            |]
            let parseRet = parseC (encodeUtf8 code) $ initPos "example.c"
            parseRet `shouldSatisfy` isRight
            let Right cTransUnit = parseRet
            -- pPrint cTransUnit
            -- 1) Extract enum and typedef name
            case cTransUnit of
                CTranslUnit cExtDecls _ ->
                    case cExtDecls of
                        [ CDeclExt cDecl ] ->
                            case cDecl of
                                CDecl cDeclSpecs cDeclInfo _ ->
                                    case cDeclSpecs of
                                        [ CStorageSpec (CTypedef _), CTypeSpec cTySpec ] ->
                                            case cTySpec of
                                                CEnumType (CEnum enumIdent enumEntries _ _) _ ->
                                                    case enumEntries of
                                                        Just entries ->
                                                            pPrint (case enumIdent of Just (Ident ident _ _) -> ident, map (\(Ident ident _ _, Just (CConst (CIntConst (CInteger value _ _) _))) -> (ident, "0x" ++ showHex value "")) entries)
            -- 2) Extract enum entries: (name, value)
        it "Parses Opaque Types" $ do
            let code = [text|
                typedef struct WGPUAdapterImpl* WGPUAdapter;
            |]
            let parseRet = parseC (encodeUtf8 code) $ initPos "example.c"
            parseRet `shouldSatisfy` isRight
            let Right cTransUnit = parseRet
            case cTransUnit of
                CTranslUnit [ cDeclExt ] _ ->
                    case cDeclExt of
                        CDeclExt (CDecl [ CStorageSpec (CTypedef _), CTypeSpec (CSUType (CStruct CStructTag (Just (Ident identImpl _ _)) _ _ _) _) ] [ (Just (CDeclr (Just (Ident ident _ _)) [CPtrDeclr [] _] _ _ _), _, _) ] _)
                            | identImpl == ident ++ "Impl" -> pPrint ident
                            | otherwise -> pPrint "Failure"
            -- 1) Extract enum and typedef name
            -- case cTransUnit of
            --     CTranslUnit cExtDecls _ ->
            --         case cExtDecls of
            --             [ CDeclExt cDecl ] ->
            --                 case cDecl of
            --                     CDecl cDeclSpecs cDeclInfo _ ->
            --                         case cDeclSpecs of
            --                             [ CStorageSpec (CTypedef _), CTypeSpec cTySpec ] ->
            --                                 case cTySpec of
            --                                     CEnumType (CEnum enumIdent enumEntries _ _) _ ->
            --                                         case enumEntries of
            --                                             Just entries ->
            --                                                 pPrint (case enumIdent of Just (Ident ident _ _) -> ident, map (\(Ident ident _ _, Just (CConst (CIntConst (CInteger value _ _) _))) -> (ident, "0x" ++ showHex value "")) entries)

        it "Parses Function Declarations" $ do
            let code = [text|
            typedef long uint32_t;
            typedef uint32_t WGPUFeatureName;
            typedef uint32_t WGPUAdapter;
            uint32_t wgpuAdapterEnumerateFeatures(WGPUAdapter adapter, WGPUFeatureName* features);
            |]
            let parseRet = parseC (encodeUtf8 code) $ initPos "example.c"
            parseRet `shouldSatisfy` isRight
            let Right cTransUnit = parseRet
            -- pPrint cTransUnit
            case cTransUnit of
                CTranslUnit [ _, _, _, cDeclExt ] _ ->
                    case cDeclExt of
                        CDeclExt (CDecl [ CTypeSpec (CTypeDef (Ident retIdent _ _) _) ] [ (Just (CDeclr (Just (Ident ident _ _)) [CFunDeclr (Right (params, False)) _ _] _ _ _), _, _) ] _) ->
                            pPrint (retIdent, ident, map (\(CDecl [CTypeSpec (CTypeDef (Ident identTy _ _) _)] [ (Just (CDeclr (Just (Ident ident _ _)) ptr _ _ _), _, _) ] _) -> (identTy ++ if isPtr ptr then " *" else "", ident)) params)
            --                 | identImpl == ident ++ "Impl" -> pPrint ident
            --                 | otherwise -> pPrint "Failure"
    where
        isPtr [] = False
        isPtr (CPtrDeclr _ _:_) = True
        isPtr (_:xs) = isPtr xs

data IntSign = Signed | Unsigned
data IntSize = I8 | I16 | I32 | I64

data Ty = EnumTy String | StructTy String | StringTy {- i.e. char * -} | IntTy IntSign IntSize | PointerTy Ty | BoolTy | FloatTy

data Enum = Enum { enumName :: String, enumFields :: [(String, Maybe Integer)] }

data Struct = Struct { structName :: String, structFields :: [(String, Ty)] }

-- data FnPtrTy = FnPtrTy { fnName :: String, fnRet :: Ty, fnParams :: [(String, Ty)] }

data State = State { stateOpaqueTypes :: [String], stateTypeSynonyms :: Map String Ty, stateEnums :: Map String Enum, stateStructs :: Map String Struct }

-- handle :: CExternalDeclaration a -> WriterT 
