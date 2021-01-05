{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.OAuth
-- Copyright   : (c) David Johnson, Avi Press, 2019
-- Maintainer  : tech@scarf.sh
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.OAuth
    ( -- * API
      getOAuth
      -- * Types
    , GetOAuth
    ) where

import           Web.Stripe.StripeRequest (Method (POST), StripeHasParam,
                                           StripeRequest (..), StripeReturn,
                                           mkStripeRequest,
                                           mkStripeRequestToHost, toStripeParam)
import           Web.Stripe.Types         (AuthCode (..), OAuth (..),
                                           Scope (..))

data GetOAuth
type instance StripeReturn GetOAuth = OAuth
instance StripeHasParam GetOAuth AuthCode
instance StripeHasParam GetOAuth Scope

getOAuth :: AuthCode -> StripeRequest GetOAuth
getOAuth authCode = request
  where request = mkStripeRequestToHost POST "connect.stripe.com" url params
        url     = "oauth/token"
        params  = toStripeParam authCode []
