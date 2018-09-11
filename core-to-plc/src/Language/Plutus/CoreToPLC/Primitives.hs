{-# LANGUAGE TemplateHaskell #-}

-- | Primitive names that correspond to Plutus Core primitives.
module Language.Plutus.CoreToPLC.Primitives where

import           Language.Plutus.CoreToPLC.Error

import           Control.Monad
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes)
import           GHC.Natural
import qualified GhcPlugins                      as GHC
import qualified Language.Haskell.TH.Syntax      as TH
import qualified Language.PlutusCore             as PC

-- | An abstract data type representing bytestrings in Plutus Core.
data ByteString

haskellIntSize :: Natural
haskellIntSize = 64

haskellBSSize :: Natural
haskellBSSize = 64

instSize :: Natural -> PC.Term tyname name () -> PC.Term tyname name ()
instSize n t = PC.TyInst () t (PC.TyInt () n)

appSize :: Natural -> PC.Type tyname () -> PC.Type tyname ()
appSize n t = PC.TyApp () t (PC.TyInt () n)

mkConstant :: PC.BuiltinName -> PC.Term tyname name ()
mkConstant n = PC.Constant () $ PC.BuiltinName () n

-- TODO: resizing primitives? better handling of sizes?

concatenate :: ByteString -> ByteString -> ByteString
concatenate = mustBeReplaced

takeByteString :: Int -> ByteString -> ByteString
takeByteString = mustBeReplaced

dropByteString :: Int -> ByteString -> ByteString
dropByteString = mustBeReplaced

sha2_256 :: ByteString -> ByteString
sha2_256 = mustBeReplaced

sha3_256 :: ByteString -> ByteString
sha3_256 = mustBeReplaced

verifySignature :: ByteString -> ByteString -> ByteString -> Bool
verifySignature = mustBeReplaced

equalsByteString :: ByteString -> ByteString -> Bool
equalsByteString = mustBeReplaced

txhash :: ByteString
txhash = mustBeReplaced

blocknum :: Int
blocknum = mustBeReplaced

{- Note [Mapping primitives]
We want the user to be able to call the Plutus primitives as normal Haskell functions.

To do this, we provide a library of such functions, and then make a map from their TH names (which we can
derive from the real function declarations, to be sure they match up), to the implementations. We then
need to do some work in the GHC Core monad to translate those mappings into mappings from Core names.
-}

makePrimitiveMap :: [(TH.Name, a)] -> GHC.CoreM (Map.Map GHC.Name a)
makePrimitiveMap associations = do
    mapped <- forM associations $ \(name, term) -> do
        ghcNameMaybe <- GHC.thNameToGhcName name
        pure $ fmap (\ghcName -> (ghcName, term)) ghcNameMaybe
    pure $ Map.fromList (catMaybes mapped)

primitiveTermAssociations :: [(TH.Name, PC.Term PC.TyName PC.Name ())]
primitiveTermAssociations = [
    ('concatenate, instSize haskellIntSize $ mkConstant PC.Concatenate)
    , ('takeByteString, instSize haskellBSSize $ instSize haskellIntSize $ mkConstant PC.TakeByteString)
    , ('dropByteString, instSize haskellBSSize $ instSize haskellIntSize $ mkConstant PC.DropByteString)
    , ('sha2_256, instSize haskellBSSize $ mkConstant PC.SHA2)
    , ('sha3_256, instSize haskellBSSize $ mkConstant PC.SHA3)
    , ('verifySignature, instSize haskellBSSize $ instSize haskellBSSize $ instSize haskellBSSize $ mkConstant PC.VerifySignature)
    , ('equalsByteString, instSize haskellBSSize $ instSize haskellBSSize $ mkConstant PC.EqByteString)
    , ('txhash, mkConstant PC.TxHash)
    , ('blocknum, instSize haskellIntSize $ mkConstant PC.BlockNum)
    ]

primitiveTypeAssociations :: [(TH.Name, PC.Type PC.TyName ())]
primitiveTypeAssociations = [
    (''ByteString, appSize haskellBSSize $ PC.TyBuiltin () PC.TyByteString)
    ]
