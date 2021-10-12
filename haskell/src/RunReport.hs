{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}


module RunReport where

import Control.Lens ((?~),(&),(.~))
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens (key, nth)
import Data.Text.Encoding (encodeUtf8)

import GetStripeAPIKey (getStripeAPIKey)


endpoint :: String
endpoint = "https://api.stripe.com/v1/reporting/report_runs"


runRep :: IO String
runRep = do
    apiKey <- getStripeAPIKey
    let opts = defaults & header "content-type" .~ ["application/x-www-form-urlencoded"] & auth ?~ oauth2Bearer (encodeUtf8 apiKey)
    res <- postWith opts endpoint
        $ pairs
            ( "report_type" .= ("balance_change_from_activity.itemized.3" :: String))
    return $ show res

