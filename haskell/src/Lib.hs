module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda

data Payload = Payload
  { connectAccountId :: String
  } deriving (Generic)

instance FromJSON Payload
instance ToJSON Payload

handler :: Payload -> Context () -> IO (Either String Payload)
handler payload context = runHandler payload

runHandler :: Payload -> IO (Either String Payload)
runHandler payload =
    if connectAccountId payload == "test_id" then
        return (Right payload)
      else
        return (Left "Left condition has been hit")

stringToPayload :: String -> Payload
stringToPayload str = Payload str
