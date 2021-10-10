module RunReport where

import Control.Lens
import Network.AWS
import Network.AWS.S3
import Network.AWS.DynamoDB
import Network.AWS.SSM
import System.IO
import qualified System.Environment as Env

strToRegion :: String -> Region
strToRegion _ = Oregon


data RegionStr = RegionStr String
data Runenv = Runenv String

configToStr :: RegionStr -> Runenv -> String
configToStr (RegionStr region) (Runenv runenv) =
    "[EnvConfig] {\n  region = " ++ region ++ "\n  runenv = " ++ runenv ++ "\n}"

runIt :: IO GetParameterResponse
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
    return res




