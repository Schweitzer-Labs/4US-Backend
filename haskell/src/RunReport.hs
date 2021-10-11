{-# LANGUAGE OverloadedStrings #-}

module RunReport where

import Control.Lens
import Network.AWS
import Network.AWS.S3
import Network.AWS.DynamoDB
import Network.AWS.SSM
import System.IO
import Data.Text as Text
import qualified System.Environment as Env
import Web.Stripe
import Web.Stripe.Customer
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding as Encode

strToRegion :: String -> Region
strToRegion _ = Oregon


data RegionStr = RegionStr String
data Runenv = Runenv String

configToStr :: RegionStr -> Runenv -> String
configToStr (RegionStr region) (Runenv runenv) =
    "[EnvConfig] {\n  region = " ++ region ++ "\n  runenv = " ++ runenv ++ "\n}"

runIt :: IO Text
runIt = do
    runenv <- Env.getEnv "RUNENV"
    regionStr <- Env.getEnv "AWS_DEFAULT_REGION"
    putStrLn $ configToStr (RegionStr regionStr) (Runenv runenv)
    lgr  <- newLogger Debug stdout
    env  <- newEnv Discover
    res  <- runResourceT
        $ runAWS (env & envLogger .~ lgr)
        $ within (strToRegion regionStr)
        $ send
        $ getParameter "/qa/lambda/stripe/apikey" & gWithDecryption .~ Just True
    stripeAPIKey <- case res ^. gprsParameter of
        Just parameter ->
          case parameter ^. pValue of
            Just secret -> do
                return secret
            Nothing -> return "nope"
        Nothing ->
          return "nope"
    let config = StripeConfig (StripeKey $ Encode.encodeUtf8 stripeAPIKey) Nothing
    result <- stripe config $ getCustomers
    case result of
        Right stripelist -> print (list stripelist :: [Customer])
        Left stripeError -> print stripeError
    return stripeAPIKey
