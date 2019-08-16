{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Effects.ExposeEndpoint where

import           Data.Proxy
import           Data.Row
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           GHC.TypeLits                            (Symbol, symbolVal)

import           Language.Plutus.Contract.Request        as Req
import           Language.Plutus.Contract.Rows.Instances (Event (..), Hooks (..))

newtype EndpointDescription = EndpointDescription String
    deriving (Eq, Ord)

type EndpointReq s = s .== Set EndpointDescription
type EndpointResp s a = s .== a
type HasEndpoint s a ρ σ =
    ( HasType s (Set EndpointDescription) σ
    , HasType s a ρ)

-- | Expose an endpoint, return the data that was entered
endpoint :: forall s a. (KnownSymbol s) => Contract (EndpointResp s a) (EndpointReq s) a
endpoint = mkRequest s Just where
  s = Set.singleton $ EndpointDescription $ symbolVal (Proxy @s)

event
  :: forall (s :: Symbol) ρ a. (KnownSymbol s, HasType s a ρ, AllUniqueLabels ρ)
  => a
  -> Event ρ
event = Event . IsJust (Label @s)

isActive
  :: forall (s :: Symbol) ρ. (KnownSymbol s, HasType s (Set EndpointDescription) ρ)
  => Hooks ρ
  -> Bool
isActive (Hooks r) = not $ Set.null $ r .! Label @s
