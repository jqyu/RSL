module Main where

import Prelude
import Control.Monad.Aff 
import Control.Monad.Aff.Console as Aff 
import Control.Monad.Eff
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref

import RSL.Core
import Services.Radredis

n1 :: RSL Int
n1 = dataFetch (Echo 1 "foo")

n2 :: RSL Int
n2 = dataFetch (DoubleEcho 2 "bar")

n3 :: RSL Int
n3 = dataFetch (HelloWorld 3)

n4 :: RSL Int
n4 = uncachedRequest (Echo 3 "qux")

add2 :: Int -> Int -> Int
add2 x y = x + y

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

sum :: RSL Int
sum = do
  s'' <- add2 <$> n1 <*> n2
  s' <- add3 <$> n1 <*> n3 <*> n4
  s <- add3 s'' s' <$> n4
  return s

test :: forall e. Aff ( console :: CONSOLE, ref :: REF | e ) Unit
test = do
  env <- liftEff emptyEnv
  val <- runRSL env sum
  Aff.log "Result is:"
  Aff.log $ show val

main :: forall e. Eff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF | e) Unit
main = do
  launchAff test
  log "Hello sailor!"
