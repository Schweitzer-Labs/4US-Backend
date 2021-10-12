{-# LANGUAGE OverloadedStrings #-}


module RunReport where

import Control.Lens ((?~),(&),(.~))
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens (key, nth)
import Data.Text.Encoding (encodeUtf8)
import Data.Int
import Now (nowEpoch, secsAgo)

import GetStripeAPIKey (getStripeAPIKey)


endpoint :: String
endpoint = "https://api.stripe.com/v1/reporting/report_runs"

runReport :: String -> IO String
runReport connectAccountId = do
    apiKey <- getStripeAPIKey
    let opts = defaults & header "content-type" .~ ["application/x-www-form-urlencoded"] & auth ?~ oauth2Bearer (encodeUtf8 apiKey)
    now <- nowEpoch
    let endTime = now - 60 * 60 * 24
    let startTime = endTime - 60 * 60 * 24 * 30
    res <- postWith opts endpoint $ formParams startTime endTime connectAccountId
    return $ show res



formParams :: Int64 -> Int64 -> String -> [FormParam]
formParams startTime endTime connectAccountId =
  [ "report_type" := ("connected_account_payout_reconciliation.itemized.5" :: String)
  , "parameters[interval_start]" := startTime
  , "parameters[interval_end]" := endTime
  , "parameters[connected_account]" := connectAccountId
  , "parameters[columns][]" := ("automatic_payout_id" :: String)
  , "parameters[columns][]" := ("automatic_payout_effective_at_utc" :: String)
  , "parameters[columns][]" := ("gross" :: String)
  , "parameters[columns][]" := ("balance_transaction_id" :: String)
  , "parameters[columns][]" := ("payment_intent_id" :: String)
  , "parameters[columns][]" := ("charge_id" :: String)
  , "parameters[columns][]" := ("connected_account" :: String)
  ]
