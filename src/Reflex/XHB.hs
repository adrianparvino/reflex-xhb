{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts, TypeFamilies, GADTs #-}

module Reflex.XHB
  ( XHB
  , XHBState (..)
  , mapRequest
  , unmapNotify
  , destroyNotify
  , runXHB
  , mapWindows
  , unmapWindows
  , changeWindowAttributes
  , setEventMask
  , positionWindows
  , resizeWindows
  , configureWindows

  -- re-exports from XHB
  , CW (..)
  , ConfigWindow (..)
  , EventMask (..)
  , WINDOW
  , getRoot

  , asks

  , module Reflex
  ) where

import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum

import Control.Arrow

import Graphics.XHB hiding (Event, changeWindowAttributes, mapWindow, unmapWindow, configureWindow)
import qualified Graphics.XHB as XHB
import qualified Graphics.XHB.Gen.RandR as RandR

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Maybe
import Data.IORef

import Control.Monad.Loops
import Data.Maybe

import Data.Word
import Data.Foldable
import Data.Traversable
import Data.Function.Pointless

data XHBState t = XHBState { connection :: Connection
                           , someEvent :: Event t SomeEvent
                           , someError :: Event t SomeError
                           }

type XHB t m = ( ReflexHost t
               , PrimMonad (HostFrame t)
               , MonadIO (HostFrame t)
               , Ref m ~ Ref IO
               , MonadHold t m
               , MonadFix m
               , MonadReader (XHBState t) m
               , PerformEvent t m
               , TriggerEvent t m
               , PostBuild t m
               , MonadIO (Performable m)
               , MonadSample t (Performable m)
               , MonadIO m
               )

runXHB :: (forall t m. XHB t m => m ()) -> IO ()
runXHB xhb = runSpiderHost $ do
  Just connection <- liftIO connect -- TODO: Safer way

  liftIO $ do
    receipt <- RandR.queryVersion connection 1 2
    getReply receipt

  (eventE, eventTriggerRef) <- newEventWithTriggerRef
  (errorE, errorTriggerRef) <- newEventWithTriggerRef
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

  chan <- liftIO newChan
  tchan <- liftIO newTChanIO

  ((), FireCommand fire) <- hostPerformEventT $ flip runPostBuildT postBuild $ flip runTriggerEventT chan (runReaderT xhb (XHBState connection eventE errorE))

  liftIO $ forkIO $ forever $ do
    es <- readChan chan
    atomically . writeTChan tchan $ es

  Just postBuildTrigger <- iterateUntil isJust (liftIO $ readIORef postBuildTriggerRef)
  fire [postBuildTrigger :=> Identity ()] $ pure ()

  forever $ do
    liftIO $ threadDelay 1000 -- TODO: Learn how to do blocking IO.
    error <- runMaybeT $ do
      errorTrigger <- MaybeT . liftIO $ readIORef errorTriggerRef
      error <- MaybeT . liftIO $ pollForError connection
      liftIO $ print error
      pure (errorTrigger, error)

    event <- runMaybeT $ do
      eventTrigger <- MaybeT . liftIO $ readIORef eventTriggerRef
      event <- MaybeT . liftIO $ pollForEvent connection
      pure (eventTrigger, event)

    runMaybeT $ do
      triggers <- MaybeT . liftIO . atomically $ tryReadTChan tchan
      (events, cbs) <- unzip . catMaybes <$> (liftIO . traverse (\((EventTriggerRef triggerRef) :=> (TriggerInvocation v cb)) -> do
                                                                     mTrigger <- readIORef triggerRef
                                                                     pure $ (\trigger -> (trigger :=> Identity v, cb)) <$> mTrigger) $ triggers)
      lift $ fire events $ return ()
      liftIO $ sequence_ cbs
      --lift $ for triggers (\((EventTriggerRef triggerRef) :=> (TriggerInvocation v cb)) ->
      --                        runMaybeT $ do
      --                          trigger <- MaybeT . liftIO $ readIORef triggerRef
      --                          lift $ fire [ trigger :=> Identity v ] $ pure ()
      --                          liftIO cb
      --                        )

    fire (catMaybes [ uncurry (\trigger -> (trigger :=>) . Identity) <$> event
                    , uncurry (\trigger -> (trigger :=>) . Identity) <$> error ]) $ pure ()

unmapNotify :: XHB t m => m (Event t UnmapNotifyEvent)
unmapNotify = do
  e <- asks someEvent
  pure $ fmapMaybe fromEvent e

mapRequest :: XHB t m => m (Event t MapRequestEvent)
mapRequest = do
  e <- asks someEvent
  pure $ fmapMaybe fromEvent e

destroyNotify :: XHB t m => m (Event t DestroyNotifyEvent)
destroyNotify = do
  e <- asks someEvent
  pure $ fmapMaybe fromEvent e

mapWindows :: (XHB t m, Traversable f) => Event t (f WINDOW) -> m ()
mapWindows e = do
  connection <- asks connection
  performEvent_ $ void . traverse (liftIO . XHB.mapWindow connection) <$> e

unmapWindows :: (XHB t m, Traversable f) => Event t (f WINDOW) -> m ()
unmapWindows e = do
  connection <- asks connection
  performEvent_ $ void . traverse (liftIO . XHB.unmapWindow connection) <$> e

changeWindowAttributes :: XHB t m => Event t (WINDOW, ValueParam Word32) -> m ()
changeWindowAttributes parameters = do
  connection <- asks connection
  performEvent_ $ uncurry (liftIO .: XHB.changeWindowAttributes connection) <$> parameters

setEventMask :: XHB t m => WINDOW -> Event t [EventMask] -> m ()
setEventMask window = changeWindowAttributes . fmap (\masks -> (window, toValueParam [(CWEventMask, toMask masks)]))

configureWindows :: (XHB t m, Traversable f) => Event t (f (WINDOW, ValueParam Word16)) -> m ()
configureWindows parameterss = do
  connection <- asks connection
  performEvent_ $ liftIO . traverse_ (uncurry $ XHB.configureWindow connection) <$> parameterss

positionWindows :: (XHB t m, Traversable f, Integral a) => Event t (f (WINDOW, (a,a))) -> m ()
positionWindows = configureWindows . fmap (fmap $ second $ \(x,y) -> toValueParam [ (ConfigWindowX, fromIntegral x)
                                                                                 , (ConfigWindowY, fromIntegral y)])

resizeWindows :: (XHB t m, Traversable f, Integral a) => Event t (f (WINDOW, (a, a))) -> m ()
resizeWindows = configureWindows . fmap (fmap $ second $ \(w,h) -> toValueParam [ (ConfigWindowWidth,  fromIntegral w)
                                                                                , (ConfigWindowHeight, fromIntegral h) ])
