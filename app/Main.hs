{-# LANGUAGE RankNTypes, OverloadedStrings, FlexibleContexts, GADTs, TupleSections, ParallelListComp #-}

module Main where

import Reflex
import Reflex.XHB
import Reflex.XHB.Util

import Control.Concurrent
import Control.Monad.Reader
import Data.Function.Pointless

import qualified Data.Set as Set

w :: Int
w = 480
h :: Int
h = 360

tallPos :: (Int, Int) -> [WINDOW] -> [(Int,Int)]
tallPos (w, h) (_:xs) =  (0,0):[ (w`div`2, h' `div` length xs) | _ <- xs | h' <- [0,h..]]
tallPos (w, h) [] = []

tallSize :: (Int, Int) -> [WINDOW] -> [(Int,Int)]
tallSize (w, h) [x] = [(w,h)]
tallSize (w, h) (_:xs) = (w`div`2, h):((w `div` 2, h `div` length xs) <$ xs)
tallSize (w, h) [] = []

gaps = 5

gapsSize :: [(Int,Int)] -> [(Int,Int)]
gapsSize = fmap (\(x,y) -> (x - 2*gaps, y - 2*gaps))

gapsPosition :: [(Int,Int)] -> [(Int,Int)]
gapsPosition = fmap (\(x,y) -> (x + gaps, y + gaps))

tall :: (Int, Int) -> [WINDOW] -> [((Int,Int), (Int, Int))]
tall dim = zip <$> tallPos dim <*> tallSize dim

main :: IO ()
main = runXHB $ do
  connection <- asks connection
  postBuild <- getPostBuild
  setEventMask (getRoot connection) $ [EventMaskSubstructureNotify, EventMaskSubstructureRedirect] <$ postBuild

  windows <- windows

  screenSizes <- fmap head <$> getScreenSizes

  let positions = zipDynWith (gapsPosition .: tallPos  .^ Set.elems) screenSizes windows
      sizes     = zipDynWith (gapsSize     .: tallSize .^ Set.elems) screenSizes windows

  resizeWindows   . updated $ zipDynWith (zip . Set.elems) windows sizes
  positionWindows . updated $ zipDynWith (zip . Set.elems) windows positions
