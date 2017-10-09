{-# LANGUAGE RankNTypes, OverloadedStrings, FlexibleContexts, GADTs, RecordWildCards #-}

module Reflex.XHB.Util
  where

import Reflex.XHB
import Graphics.XHB ( window_MapRequestEvent
                    , window_UnmapNotifyEvent
                    , getReply
                    )
import Graphics.XHB.Gen.RandR

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans

import Data.Either
import Data.List
import Data.These
import Data.Align

import qualified Data.Set as Set

windows :: XHB t m => m (Dynamic t (Set.Set WINDOW))
windows = do
  connection <- asks connection
  mr <- fmap window_MapRequestEvent <$> mapRequest
  umr <- fmap window_UnmapNotifyEvent <$> unmapNotify

  mapWindows   $ (:[]) <$> mr
  unmapWindows $ (:[]) <$> umr

  performEvent_ $ liftIO . putStrLn . ('m':) . show <$> mr
  performEvent_ $ liftIO . putStrLn . ('u':) . show <$> umr

  accum (\a x -> mergeTheseWith Set.delete Set.insert (.) x a) Set.empty $ align umr mr

getScreenSizes :: (XHB t m, Integral a) => m (Dynamic t [(a, a)])
getScreenSizes = do
  connection <- asks connection
  postBuild <- getPostBuild
  reply <- performEventAsync $
    liftIO . (\send -> void . forkIO . void . runEitherT $ do
        receipt <- lift $ getScreenResources connection (getRoot connection)
        MkGetScreenResourcesReply {..} <- EitherT $ getReply receipt
        lift $ do
          receipts <- traverse (\c -> getCrtcInfo connection c config_timestamp_GetScreenResourcesReply) crtcs_GetScreenResourcesReply
          replies <- rights <$> traverse getReply receipts
          send replies) <$ postBuild

  holdDyn [(0,0)] $ fmap (join (***) fromIntegral . (width_GetCrtcInfoReply &&& height_GetCrtcInfoReply)) <$> reply
  -- TODO: Check for updates.
