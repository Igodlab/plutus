{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
module Evaluation.Generator
    ( max_size
    , hoistSupply
    , genSizeDef
    , typedBuiltinAsValue
    , GenPlcT
    , runPlcT
    , IterAppValue(..)
    , genTypedBuiltin
    , genIterAppValue
    , genTypedBuiltinPair
    , genConstantSized
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Evaluation.Constant.GenTypedBuiltinSized

import           Control.Monad.Reader
import           Control.Monad.Morph
import           Data.Text.Prettyprint.Doc
import           Hedgehog hiding (Size, Var, annotate)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

max_size :: Size
max_size = 128

hoistSupply :: (MFunctor t, Monad m) => r -> t (ReaderT r m) a -> t m a
hoistSupply r = hoist $ flip runReaderT r

genSizeDef :: Monad m => GenT m Size
genSizeDef = Gen.integral $ Range.exponential 1 max_size

-- | Coerce a Haskell value to a PLC value checking all constraints
-- (e.g. an 'Integer' is in appropriate bounds) along the way and
-- fail in case constraints are not satisfied.
typedBuiltinAsValue :: Monad m => TypedBuiltin Size a -> a -> GenT m (Value TyName Name ())
typedBuiltinAsValue tb x = maybe (error err) return $ makeConstant tb x where
    sx = prettyString $ TypedBuiltinValue tb x
    err = "prop_typedAddInteger: out of bounds: " ++ sx

-- | The type used in generators defined in this module.
-- It is parameterized by an 'TheGenTypedBuiltinSized' which determines
-- how to generate sized builtins having a 'Size'. See for example
-- 'genTypedBuiltinSizedSum' and 'genTypedBuiltinSizedDiv'.
type GenPlcT m = GenT (ReaderT (TheGenTypedBuiltinSizedT m) m)

data IterAppValue head arg r = IterAppValue
    (Term TyName Name ())
    (IterApp head arg)
    (TypedBuiltinValue Size r)

instance (Pretty head, Pretty arg) => Pretty (IterAppValue head arg r) where
    pretty (IterAppValue term pia tbv) = parens $ mconcat
        [ "As a term: ", pretty term, line
        , "As an iterated application: ", pretty pia, line
        , "As a value: ", pretty tbv
        ]

runPlcT :: Monad m => GenTypedBuiltinSizedT m -> GenPlcT m a -> GenT m a
runPlcT genTbs = hoistSupply $ TheGenTypedBuiltinSized genTbs

-- | Generate a value of one of the builtin types.
-- See 'TypedBuiltin' for the list of such types.
genTypedBuiltin :: Monad m => TypedBuiltin Size a -> GenPlcT m a
genTypedBuiltin (TypedBuiltinSized sizeEntry tbs) = do
    let size = flattenSizeEntry sizeEntry
    TheGenTypedBuiltinSized genTbs <- ask
    hoist lift $ genTbs size tbs
genTypedBuiltin TypedBuiltinBool                  = Gen.bool

-- | Generate a value of one of the builtin types (see 'TypedBuiltin' for
-- the list of such types) and return it along with the corresponding PLC value.
genTypedBuiltinPair :: Monad m => TypedBuiltin Size a -> GenPlcT m (a, Term TyName Name ())
genTypedBuiltinPair tb = do
    x <- genTypedBuiltin tb
    v <- typedBuiltinAsValue tb x
    return (x, v)

-- | Generate a value out of a 'TypeScheme' and return it along with the corresponding PLC value.
genSchemedPair :: Monad m => TypeScheme Size a r -> GenPlcT m (a, Term TyName Name ())
genSchemedPair (TypeSchemeBuiltin tb) = genTypedBuiltinPair tb
genSchemedPair (TypeSchemeArrow _ _)  = error "Not implemented."
genSchemedPair (TypeSchemeAllSize _)  = error "Not implemented."

genIterAppValue
    :: forall head a r m. Monad m
    => (head -> Term TyName Name ())
    -> Typed head a r  -- ^ A (typed) builtin name to apply.
    -> a               -- ^ The semantics of the builtin name. E.g. the semantics of
                       -- 'AddInteger' (and hence 'typedAddInteger') is '(+)'.
    -> GenPlcT m (IterAppValue head (Term TyName Name ()) r)
genIterAppValue embHead (Typed h schema) op = go schema (embHead h) id op where
    go
        :: TypeScheme Size c r
        -> Term TyName Name ()
        -> ([Value TyName Name ()] -> [Value TyName Name ()])
        -> c
        -> GenPlcT m (IterAppValue head (Term TyName Name ()) r)
    go (TypeSchemeBuiltin builtin) term args y = do  -- Computed the result.
        let pia = IterApp h $ args []
            tbv = TypedBuiltinValue builtin y
        return $ IterAppValue term pia tbv
    go (TypeSchemeArrow schA schB) term args f = do  -- Another argument is required.
        (x, v) <- genSchemedPair schA                -- Get a Haskell and the correspoding PLC values.

        let term' = Apply () term v                  -- Apply the term to the PLC value.
            args' = args . (v :)                     -- Append the PLC value to the spine.
            y     = f x                              -- Apply the Haskell function to the generated argument.
        go schB term' args' y
    go (TypeSchemeAllSize schK)    term args f = do
        size <- genSizeDef                           -- Generate a size.
        let term' = TyInst () term $ TyInt () size   -- Instantiate the term with the generated size.
        go (schK size) term' args f                  -- Instantiate a size variable with the generated size.

genConstantSized :: Size -> Gen (Constant ())
genConstantSized size = Gen.choice
    [ BuiltinInt () size <$> genTypedBuiltinSizedDef size TypedBuiltinSizedInt
    , BuiltinBS  () size <$> genTypedBuiltinSizedDef size TypedBuiltinSizedBS
    , return $ BuiltinSize () size
    ]
