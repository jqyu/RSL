module Main where

import Prelude
import Control.Monad.Aff 
import Control.Monad.Aff.Console as Aff 
import Control.Monad.Eff
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref

import RSL.Core as RSL
import Services.Radredis

test :: forall e. Aff ( console :: CONSOLE, ref :: REF | e ) Unit
test = do
  env <- liftEff RSL.emptyEnv
  num <- RSL.runRSL env (RSL.dataFetch (Echo 1 "test"))
  Aff.log "Result is:"
  Aff.log $ show num

main :: forall e. Eff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF | e) Unit
main = do
  launchAff test
  log "Hello sailor!"
