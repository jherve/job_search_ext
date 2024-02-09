module LinkedIn.Loadable where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..), isRight)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import LinkedIn.QueryRunner (queryOne)
import LinkedIn.Queryable (class Queryable)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import sleepImpl :: Int -> Effect (Promise Unit)

sleep :: Int -> Aff Unit
sleep = sleepImpl >>> toAffE

waitForElement' ∷ ∀ q. Queryable q ⇒ Int → Int → String → q → Effect Unit
waitForElement' loopDelay maxLoops selector q = launchAff_ do
  res <- waitForElement loopDelay maxLoops selector q
  case res of
    Left l -> log $ "failed after " <> show l
    Right r -> log $ "succeeded after " <> show r

waitForElement ∷ ∀ q. Queryable q ⇒ Int → Int → String → q → Aff (Either Int Int)
waitForElement loopDelay maxLoops selector q = waitingLoop 0
  where
    waitingLoop :: Int -> Aff (Either Int Int)
    waitingLoop iter | iter >= maxLoops = pure $ Left iter
    waitingLoop iter = do
      n <- liftEffect $ runExceptT $ queryOne selector q
      case n of
        Right _ -> pure $ Right iter
        Left _ -> do
          sleep loopDelay
          waitingLoop (iter + 1)

-- Returns true if the element was found after waiting for maxLoops
waitFor ∷ ∀ q. Queryable q ⇒ Int → Int → String → q → Aff Boolean
waitFor loopDelay maxLoops selector q = pure <<< isRight =<< waitForElement loopDelay maxLoops selector q
