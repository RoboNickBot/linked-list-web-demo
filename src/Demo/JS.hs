{-# LANGUAGE CPP, OverloadedStrings #-}
module Demo.JS ( readInputState
               , writeInputState
               , mkRandomInput
               , sDrawButton
               , printHighError
               , sCellsDiv
               , sNumCells
               , printLowError
               , cullErrors
               , mark
               , sRandomButton 
               , sLowerControls
               , drawList
               , placeValues
               , displayOutput
               , scaleMax
               , sCellGen
               , getGenInfo
               , mkCanvas ) where

import Control.Monad
import Control.Applicative
import Text.Read (readMaybe)
import JavaScript.JQuery
import JavaScript.Canvas hiding (Left, Right)
import GHCJS.Types
import GHCJS.Foreign
import Data.Text (pack, unpack, Text)
import qualified Data.Map as M (empty, insert)
import Data.Maybe (fromJust)
import Demo.Types
import Demo.Links
import Demo.Random

-- Easily Configurable!
canvasXPadding = 1 :: Double
canvasYPadding = 1 :: Double
scaleMax = 100 :: Double
minCanHeight = 160

sLowerControls = select "#c"
sNumCells = select "#numcells"
sStartHead = select "#starthead"
sCellGen = select "#generatenew"
sRandomButton = select "#randomButton"
sSizeDiv = select "#size"
sStartDiv = select "#start"
sCellsDiv = select "#boxbox"
sDrawButton = select "#drawButton"
sHeadInput = select "#head"
sCanvasBox = select "#drawingbox"
sCanvas = select "#theCanvas" -- dont forget to make it!
sCellNum i = select (pack (template (cellMkName i)))
  where template n = "<div class=\"outer\"><div class=\"inner\">" 
                     ++ (show i) 
                     ++ "</div><input id=\"" 
                     ++ n 
                     ++ "\" type=\"text\" name=\"a\" /></div>"

mark :: (Bool, [Int]) -> IO ()
mark (b,is) = markHead b >> unMarkCells >> markCells is

markHead :: Bool -> IO ()
markHead b = 
  if b
     then sHeadInput 
          >>= setAttr "style" "border-color: red;" 
          >> return ()
     else sHeadInput
          >>= setAttr "style" "border-color: black;"
          >> return ()

unMarkCells = do start <- pullVal sStartDiv
                 size <- pullVal sSizeDiv
                 let f a = select (pack ("#hey" ++ (show a)))
                           >>= setAttr "style" "border-color: black;"
                           >> return ()
                     r i s = if i < s
                                then f i >> r (i + 1) s
                                else return ()
                 r start (start + size)

markCells is = do let r :: [Int] -> IO ()
                      r (i:is) = f i >> r is
                      r [] = return ()
                      f a = select (pack ("#hey" ++ (show a)))
                            >>= setAttr "style" "border-color: red;"
                            >> return ()
                  r is

getGenInfo :: IO (Either String (Int, Int))
getGenInfo = 
  do start <- fmap unpack (sStartHead >>= getVal)
     size <- fmap unpack (sNumCells >>= getVal) 
     case (readMaybe start, readMaybe size) of
       (Nothing,_) -> return (Left "\"Starting Index\" requires Integer")
       (Just _, Nothing) -> 
         return (Left "\"Number of Memory Cells\" requires Integer")
       (Just i, Just s) -> return (Right (i,s))

placeValues :: Int -> Int -> IO ()
placeValues start size = 
  let f = (pack . show)
      s val jq = jq >>= setVal (f val)
  in s start sStartHead >> s size sNumCells >> return ()

{- There are two of these because when you are creating elements,
   you must omit the "#" that you use to later select them.
   
   I was using the "#" in the name to both create AND select them
   before, and was getting a "TypeError" in the firefox console
   as a result. Almost drove me crazy :/   -}
cellName :: Int -> String
cellName i = "#hey" ++ (show i)
cellMkName :: Int -> String
cellMkName i = "hey" ++ (show i)

printList :: Either String [LElem] -> IO ()
printList = print . show

showVal sel = fmap (print . unpack) (sel >>= getVal)
pullVal :: IO JQuery -> IO Int
pullVal sel = do s <- fmap unpack (sel >>= getVal)
                 print $ "this pullval: " ++ s
                 return (read s)

readInputState :: IO InputState
readInputState = do start <- pullVal sStartDiv
                    size <- pullVal sSizeDiv
                    print "readinputstate"
                    h <- getHead
                    m <- getMemSt start size 
                    return (InSt start size h m)

mkRandomInput :: IO InputState
mkRandomInput = do showVal sStartDiv
                   showVal sSizeDiv
                   
                   start <- pullVal sStartDiv
                   size <- pullVal sSizeDiv
                   print (pack ("mkrandom " ++ (show start) ++ ":" ++ (show size)))
                   ri <- randomInput start size
                   writeInputState ri
                   return ri

getHead :: IO String
getHead = fmap unpack (getVal =<< sHeadInput)

getMemSt :: Int -> Int -> IO MemSt
getMemSt start size = fmap mkMemSt (r start)
  where r i = if i < (start + size)
                 then do c <- readCell i
                         fmap (c:) (r (i+1))  --liftM (:) (readCell i) (r (i+1))
                 else return []

writeInputState :: InputState -> IO ()
writeInputState (InSt i s h m) = mkBoxes i s m >> setHead h

setHead :: String -> IO ()
setHead h = sHeadInput >>= setVal (pack h) >> return ()

readMemSt :: [Cell] -> MemSt
readMemSt = foldr (\(i,s) -> M.insert i (i,s)) M.empty

readCell :: Int -> IO Cell
readCell i = let name = pack (cellName i)
               in fmap (((,) i) . unpack) (print (cellName i) >> (print "ah" >> select name >>= getVal))

writeCell :: Int -> String -> IO ()
writeCell i s = select (pack (cellName i)) >>= setVal (pack s) >> return ()

mkBoxes :: Int -> Int -> MemSt -> IO ()
mkBoxes start size m = clear >> note start size >> r start 0
  where note :: Int -> Int -> IO ()
        note i s = let f x = (setVal ((pack . show) x))
                   in sSizeDiv >>= f s >> sStartDiv >>= f i >> return ()          
        r :: Int -> Int -> IO ()
        r s i = if i < size
                   then do print $ "making box number " ++ (show (s + i)) 
                           box <- sCellNum (s + i)
                           parent <- sCellsDiv
                           appendJQuery box parent
                           writeCell (s + i) (stringAtIndex (s + i) m)
                           r s (i + 1)
                   else return ()
        clear :: IO ()
        clear = sCellsDiv >>= children >>= remove >> return ()

getCanvasDimensions :: IO (Int,Int)
getCanvasDimensions = do
  sh <- getHeight =<< select "#s"
  ah <- getHeight =<< select "#a"
  bh <- getHeight =<< select "#b"
  ch <- getHeight =<< select "#c"
  dw <- getWidth =<< select "#drawingbox"
  let h = max minCanHeight (floor (sh - ah - bh - ch - 170))
      w = floor $ dw - 13 -- not sure why i need this...
  return (w,h)
  
mkCanvas :: IO ()
mkCanvas = do
  (w,h) <- getCanvasDimensions
  p <- sCanvasBox
  children p >>= remove
  c <- select $ pack $ "<canvas id=\"theCanvas\" width=\""
                       ++ show w
                       ++ "\" height=\""
                       ++ show h
                       ++ "\"></canvas>"
  appendJQuery c p
  return ()

displayOutput :: Either String Layout -> IO ()
displayOutput l = cullErrors >> case l of
                                  Left er -> printLowError er
                                  Right ls -> drawList ls

withPadding :: (Double, Double) -> (Double, Double)
withPadding (x,y) = (x - (2 * canvasXPadding), y - (2 * canvasYPadding))

addOffsets :: Double -> (Double, Double) -> Layout -> LayoutD
addOffsets scale (cx,cy) ls = foldr f [] ls
  where f (e, (x, y), os) = let sx = scale * (fromIntegral (fst (getRect ls)))
                                sy = scale * (fromIntegral (snd (getRect ls)))
                                fx = ((cx - sx) / 2) + canvasXPadding
                                fy = ((cy - sy) / 2) + canvasYPadding
                                dx = scale * (fromIntegral x)
                                dy = scale * (fromIntegral y)
                            in (:) (e, (dx + fx, dy + fy), nmap ((* scale) . fromIntegral) os)

type Coord = (Double, Double)

drawList :: Layout -> IO ()
drawList ls = do cints <- getCanvasDimensions
                 let csize = nmap fromIntegral cints
                     cdims = withPadding csize
                     scale = min scaleMax (findScale cdims (getRect ls))
                     (h,w) = csize
                 c <- sCanvas >>= indexArray 0 . castRef >>= getContext
                 save c
                 clearRect 0 0 h w c 
                 restore c
                 let dls = addOffsets scale csize ls
                     r (l:ls) = (drawElem c scale) l >> r ls
                     r _ = return ()
                 r dls

drawElem :: Context -> Double -> (DElem, (Double, Double), (Double, Double)) -> IO ()
drawElem c scale elem = 
  let ((t,i,v), (x, y), (xo, yo)) = elem
  in case t of
       Box -> do save c 
                 print ("The scale is: " ++ (show scale))
                 -- the following magic numbers were experimentally chosen...
                 lineWidth (scale * 5 / 64) c
                 strokeRect x (y + (yo / 3)) xo (yo * 2 / 3) c 
                 drawTextFloor ( (x + (xo / 2)) 
                               , (y + (yo / 3) - (yo / 9)))
                               (xo / 2) 
                               (yo / 7) 
                               i c 
                 drawTextCenter ( (x + (xo / 2)
                                , (y + (yo * 8 / 12))))
                                (xo * 4 / 5) 
                                (yo * 7 / 18) 
                                v c 
                 restore c
       Arrow -> do save c
                   
                   let endX = (x + (xo * 10 / 12))
                       endY = (y + (yo * 12 / 18))

                   lineWidth (scale * 8 / 64) c
                   beginPath c
                   moveTo (x + (xo * 2 / 12)) endY c
                   lineTo (endX - (xo * 2 / 12)) endY c
                   stroke c 
                   
                   lineWidth (scale * 2 / 64) c
                   beginPath c
                   moveTo (x + (xo * 8 / 12)) (y + (yo * 13.5 / 18)) c 
                   lineTo (endX + (xo * 0.5 / 12)) endY c
                   lineTo (x + (xo * 8 / 12)) (y + (yo * 10.5 / 18)) c
                   closePath c
                   stroke c
                   fill c 
                   
                   drawTextFloor ( (x + (xo / 2))
                                 , (y + (yo * 9.5 / 18)))
                                 (xo / 2)
                                 (yo / 7)
                                 i c

                   restore c
       LoopBack z -> do let zd = (fromIntegral z) :: Double
                        save c
                        
                        lineWidth (scale * 8 / 64) c
                        beginPath c
                        let yu = (yo / 10)
                            horiz = (y + 4 * yu)
                        moveTo (x + (xo * 2 / 12)) horiz c
                        lineTo (x + (xo / 2)) horiz c
                        lineTo (x + (xo / 2)) (horiz + 5 * yu) c
                        
                        -- Here, we move back one width and then jump
                        -- two widths at a time (arrow + box)
                        let tarX = ((x + (xo / 2)) - xo - (zd * 2 * xo))
                        
                        lineTo tarX (horiz + 5 * yu) c
                        lineTo tarX (horiz + 4 * yu) c
                        stroke c
                        
                        lineWidth (scale * 2 / 64) c
                        beginPath c
                        moveTo (tarX - (xo * 1.5 / 12)) (horiz + 4 * yu) c
                        lineTo tarX (horiz + (3 * yu)) c
                        lineTo (tarX + (xo * 1.5 / 12)) (horiz + 4 * yu) c
                        closePath c
                        stroke c
                        fill c
                        
                        drawTextFloor ( (x + (xo / 2))
                                      , (y + (yu * 10 * (3 / 5) * 9.5 / 18)))
                                      (xo / 2)
                                      (yu * 8 / (7 * (4/3)))
                                      i c
                        
                        restore c

cullErrors = select "#lowError" >>= remove
             >> select "#highError" >>= remove
             >> return ()

printHighError = printError "highError" "#b"
printLowError = printError "lowError" "#c"

printError a b e = 
  do err <- select (pack 
                      ("<p class=\"errors\" id=\"" ++ a ++ "\">Error: (" ++ e ++ ")</p>"))
     par <- select (pack b)
     appendJQuery err par
     return ()

drawTextCenter :: Coord   -- location at which to center the text
               -> Double  -- maximum width of the text
               -> Double  -- maximum height of the text
               -> String  -- the text to be drawn
               -> Context -- the canvas context
               -> IO ()
drawTextCenter (x,y) maxW maxH s c =
  do (a,b) <- setFont maxH maxW s c
     fillText (pack s) (x - (a / 2)) (y + (b / 2)) c

-- same as drawTextCenter, but floors the text at the coordinates
drawTextFloor :: Coord -> Double -> Double -> String -> Context -> IO ()
drawTextFloor (x,y) maxW maxH s c =
  do (a,_) <- setFont maxH maxW s c
     fillText (pack s) (x - (a / 2)) y c

setFont :: Double -> Double -> String -> Context -> IO (Double, Double)
setFont maxHeight maxWidth s c = try maxWidth maxHeight s c

fontPrecision = 6 -- size of steps taken when choosing a font
panicSize = 1 -- size to choose if algorithm bottoms out
try d f s c = do font (pack ((show ((floor f)::Int)) ++ "pt Calibri")) c
                 x <- measureText (pack s) c
                 if x > d
                    then if x > 0
                            then try d (f - fontPrecision) s c 
                            else print ("hit bottom..") 
                                 >> return (panicSize,f)
                    else print (show (floor f)) >> return (x,f)
