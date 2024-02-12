module ExampleWebExt.Storage where

import Prelude

import Browser.WebExt.Storage.Sync as Sync
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)

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
