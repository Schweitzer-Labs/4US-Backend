module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda
import RunReport (runReport)

data Payload = Payload
  { connectAccountId :: String
  } deriving (Generic)

instance FromJSON Payload
instance ToJSON Payload

handler :: Payload -> Context () -> IO (Either String String)
handler payload context = runHandler payload

runHandler :: Payload -> IO (Either String String)
runHandler payload = toRes payload

toRes :: Payload -> IO (Either String String)
toRes payload = do
    res <- runReport $ connectAccountId payload
    return $ Right res

stringToPayload :: String -> Payload
stringToPayload str = Payload str
