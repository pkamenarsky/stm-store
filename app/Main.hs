module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM

import Control.Monad

import Data.IORef

import GHC.Conc

import System.Random

import Debug.Trace

data Request = Inc | Done deriving Show
data Response = Response Int | Retry | Commited deriving Show

runTx :: TVar Int -> Chan Request -> Chan Response -> IO ()
runTx counter i o = do
  retrying <- newIORef False

  atomically $ do
    unsafeIOToSTM $ do
      retrying' <- readIORef retrying
      when retrying' $ writeChan o Retry
      atomicModifyIORef' retrying $ \_ -> (True, ())

    let go = do
          req <- unsafeIOToSTM $ do
            readChan i

          case req of
            Inc -> do
              cnt <- readTVar counter
              writeTVar counter (cnt + 1)

              unsafeIOToSTM $
                writeChan o (Response cnt)
              go
            Done -> return ()
    go

  writeChan o Commited

main :: IO ()
main = do
  counter <- newTVarIO 0

  forM_ [0..100] $ \_ -> do
    forkIO $ do
      i <- newChan
      o <- newChan

      forkIO $ runTx counter i o
      void $ forkIO $ go i o

  threadDelay 10000000

  cnt <- readTVarIO counter

  print cnt

  where
    go i o = do
      -- rnd <- randomRIO (0, 1000000)

      writeChan i Inc
      -- threadDelay rnd
      rsp <- readChan o
      -- threadDelay rnd
      traceIO $ show rsp

      writeChan i Done
      -- threadDelay rnd
      commited <- readChan o
      -- threadDelay rnd

      case commited of
        Retry -> do
          traceIO "Retry"
          go i o
        Commited -> do
          traceIO "Commited"
