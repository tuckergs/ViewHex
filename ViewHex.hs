{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Bits
import Data.Function
import Data.List
import Data.Word
import System.Environment
import System.Exit
import System.IO
import UI.NCurses

import HexStuff

readByte :: Handle -> IO Word8
readByte = fmap (fromIntegral . fromEnum) . hGetChar

type Offset = Integer

showViewerLine :: Offset -> [Word8] -> String
showViewerLine off bs = poff ++ " | " ++ pbs
  where
    pad l a = let unpadStr = show (Hex a) in take (l - length unpadStr) (repeat '0') ++ unpadStr
    poff = pad 8 off
    pbs = concatMap ((++ " ") . pad 2) bs

updateViewerLine :: Window -> Integer -> Offset -> Handle -> Curses ()
updateViewerLine w r off handle = do
  let 
    getC = hIsEOF handle >>= \case
      True -> return []
      _ -> fmap (:[]) $ readByte handle
  bs <- liftIO $ fmap concat $ replicateM 4 getC
  updateWindow w $ do
    moveCursor r 0
    clearLine
    drawString $ showViewerLine off bs

updateViewerAndRender :: Window -> Offset -> Handle -> Curses ()
updateViewerAndRender w off handle = do
  liftIO $ hSeek handle AbsoluteSeek off
  (numRows,_) <- updateWindow w windowSize
  flip fix (2,off) $ \loop (r,off) -> 
    if (r /= numRows - 1) 
      then updateViewerLine w r off handle >> loop (r+1,off+4)
      else return ()
  render

main = do
  args <- getArgs
  let open = flip openBinaryFile ReadMode
  (s,handle) <- case args of
    (s:[]) -> fmap ((,) s) $ open s
    _ -> die "Only one file at a time plz k thx"
  hSeek handle SeekFromEnd 0
  fileLength <- hTell handle
  runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
      moveCursor 0 0
      drawString $ "File: " ++ s
      moveCursor 1 0
      drawLineH (Just glyphLineH) 4096
    flip fix 0 $ \loop off -> do
      updateViewerAndRender w off handle
      fix $ \again -> getEvent w Nothing >>= \case
        Just (EventCharacter 'j') -> loop ((off+4) `rem` fileLength)
        Just (EventCharacter 'k') -> loop ((off-4) `mod` fileLength)
        Just (EventCharacter 'q') -> return ()
        _ -> again

        
        
      



  

prompt :: Window -> Integer -> Integer -> Curses String
prompt w r c = do
  let clearAndDisp s = updateWindow w (moveCursor r c >> clearLine >> drawString s) >> render
  flip fix "" $ \loop str -> do
    let 
      removeChar [] = []
      removeChar ls = init ls
      doABksp = loop $ removeChar str
      doARet = return str
      doACont = loop str
    clearAndDisp str
    getEvent w Nothing >>= \case
      Just (EventCharacter '\n') -> doARet
      Just (EventCharacter '\r') -> doARet
      Just (EventCharacter '\b') -> doABksp
      Just (EventCharacter ch) -> loop (str ++ [ch])
      Just (EventSpecialKey sp) -> case sp of
        KeyBackspace -> doABksp
        KeyEnter -> doARet
        _ -> loop str
      _ -> loop str

    

