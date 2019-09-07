{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Data.Word
import System.Environment
import System.Exit
import System.IO
import UI.NCurses

import HexStuff 
import ParseHelpers (numberParser, hexStringParser)
import ParseMonad

readByte :: Handle -> IO Word8
readByte = fmap (fromIntegral . fromEnum) . hGetChar

getFileLength :: Handle -> IO Integer
getFileLength handle = do
  curPos <- hTell handle
  hSeek handle SeekFromEnd 0
  hTell handle <* hSeek handle AbsoluteSeek curPos

type Offset = Integer

------ PARSERS

data OffsetLine = Absolute Offset | Relative Offset

offsetLineParser :: Parse String OffsetLine
offsetLineParser = fmap Absolute numberParser <|> (token '+' >> fmap Relative numberParser) <|> (token '-' >> fmap (Relative . negate) numberParser)


------ GUI HELPERS

-- hexStringParser is used
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
  updateWindow w $ moveCursor 2 9 
  liftIO $ hSeek handle AbsoluteSeek off
  render

search :: Integer -> Handle -> [Word8] -> IO (Maybe Offset)
search mult handle bytes = do
  let 
    relMove = hSeek handle RelativeSeek
    absMove = hSeek handle AbsoluteSeek
  curPos <- hTell handle
  getFileLength handle >>= \ln -> if not $ (0 <= curPos+4*mult) && (curPos+4*mult < ln)
    then return Nothing
    else do
      relMove $ 4*mult
      firstSearchPos <- hTell handle
      flip fix (firstSearchPos,bytes) $ \loop (searchPos,chaseBytes) -> do
        case chaseBytes of
          [] -> return $ Just searchPos
          (b:bs) -> 
            hIsEOF handle >>= \case
              True -> return Nothing
              False -> readByte handle >>= \q -> if (q==b)
                then loop (searchPos,bs) -- Keep checking whether seq matches
                else if (searchPos+mult < 0)
                  then return Nothing
                  else absMove (searchPos+mult) >> loop (searchPos+mult,bytes) -- Try the next search position
    
getInput :: Window -> (Event -> Curses a) -> Curses a
getInput w f = fix $ \again -> getEvent w Nothing >>= \case
  Nothing -> again
  Just ev -> f ev
  

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


------ MAIN

-- To force you to not forget a loop mr
data Dummy = Dummy

data MainRecord = MR { _curOff :: Integer , _prevSearch :: [Word8] , _prevMult :: Integer , _marks :: [(Char,Integer)] }
makeLenses ''MainRecord

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
    setCBreak True
    w <- defaultWindow
    updateWindow w $ do
      moveCursor 0 0
      drawString $ "File: " ++ s
      moveCursor 1 0
      drawLineH (Just glyphLineH) 4096

    flip fix (MR 0 [] 1 []) $ \loop mr -> do
      let off = view curOff mr
      updateViewerAndRender w off handle
      (numRows,_) <- updateWindow w windowSize
      let 
        offsetRound = (.&. 0xfffffffc) . (`mod` fileLength)
        myMsg str = do
          updateWindow w $ do
            moveCursor (numRows-1) 0
            clearLine
            drawString str
          return Dummy
        myPrompt c = do
          updateWindow w $ do
            moveCursor (numRows-1) 0
            drawString $ c:" "
          prompt w (numRows-1) 2
        mySearch updatePrev mult bytes = liftIO (search mult handle bytes) >>= \case
          Nothing -> myMsg "Search hit boundary" >> loop (
            set prevMult mult $
            set prevSearch bytes mr)
          Just newOff -> if updatePrev
            then loop $ 
              set curOff (offsetRound newOff) $ 
              set prevMult mult $
              set prevSearch bytes mr
            else loop $ set curOff (offsetRound newOff) mr
        mySearchAndPrompt mult c = do
          line <- myPrompt c
          case parse hexStringParser line of
            Left _ -> myMsg "Error: Couldn\'t parse your string" >> loop mr
            Right hexStr -> mySearch True mult hexStr
      getInput w $ \case
        EventCharacter 'q' -> return ()
        EventCharacter 'j' -> loop $ set curOff (offsetRound (off+4)) mr
        EventCharacter 'k' -> loop $ set curOff (offsetRound (off-4)) mr
        EventCharacter 'g' -> do
          offLine <- myPrompt 'g'
          case parse offsetLineParser offLine of
            Left _ -> myMsg "Error: Couldn\'t parse your string" >> loop mr
            Right (Absolute aOff) -> loop $ set curOff (offsetRound aOff) mr
            Right (Relative rOff) -> loop $ set curOff (offsetRound (off+rOff)) mr
        EventCharacter '/' -> mySearchAndPrompt 1 '/'
        EventCharacter '?' -> mySearchAndPrompt (-1) '?'
        EventCharacter 'n' -> mySearch False (view prevMult mr) (view prevSearch mr)
        EventCharacter 'N' -> mySearch False (negate $ view prevMult mr) (view prevSearch mr)
        EventCharacter 'm' -> 
          getInput w $ \case
            EventCharacter c -> if isAlpha c
              then do
                myMsg $ "Mark " ++ [c] ++ " written"
                -- myMsg $ show ((c,off):view marks mr)
                loop $ over marks ((c,off):) mr
              else loop mr
            _ -> loop mr
        EventCharacter '`' -> getInput w $ \case
          EventCharacter c -> if isAlpha c
            then case lookup c (view marks mr) of
              Just newOff -> loop $ set curOff (offsetRound newOff) mr
              Nothing -> do
                myMsg $ "Mark " ++ [c] ++ " not found"
                loop mr
            else loop mr
          _ -> loop mr
        _ -> loop mr

    

