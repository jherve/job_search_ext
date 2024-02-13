module ExampleWebExt.Storage where

import Prelude

import Browser.WebExt.Storage.Local as Local
import Browser.WebExt.Storage.Sync as Sync
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import ExampleWebExt.NativeMessage (NativePythonJobOffer)
import Foreign.Object (singleton)

type SyncStorage = { jobsPath :: String }

setJobsPath ∷ String → Effect Unit
setJobsPath path = Sync.set $ encodeJson {jobsPath: path}

getJobsPath ∷ Aff (Either JsonDecodeError String)
getJobsPath = do
  s <- Sync.get "jobsPath"
  let 
    asStorage :: Either JsonDecodeError SyncStorage
    asStorage = decodeJson s

  pure $ map (\{jobsPath} -> jobsPath) asStorage

clearAllJobs :: Effect Unit
clearAllJobs = Local.clear

storeJob :: NativePythonJobOffer -> Effect Unit
storeJob jo@{id} = Local.set $ encodeJson $ singleton id jo
