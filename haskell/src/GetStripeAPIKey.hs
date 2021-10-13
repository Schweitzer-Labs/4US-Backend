{-# LANGUAGE OverloadedStrings #-}

module GetStripeAPIKey where


import Network.AWS
import Network.AWS.SSM
import System.IO
import Data.Text as Text
import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding as Encode

import Control.Lens


strToRegion :: String -> Region
strToRegion _ = Oregon

data RegionStr = RegionStr String
data Runenv = Runenv String

configToStr :: RegionStr -> Runenv -> String
configToStr (RegionStr region) (Runenv runenv) =
    "[EnvConfig] {\n  region = " ++ region ++ "\n  runenv = " ++ runenv ++ "\n}"


-- @ToDo
-- use Encode.encodeUtf8 stripeAPIKey to convert Text to String
-- @ToDo
getStripeAPIKey :: IO Text
getStripeAPIKey = do
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
    return stripeAPIKey
