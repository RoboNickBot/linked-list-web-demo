--
-- This is a demo of Elm
--
-- The goal here is to properly arrange some elements on the
-- page, keeping everything appropriately spaced and sized
--

import Maybe as Maybe
import String as Str
import Window
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

-- Configurables
defaultListSize : Int
defaultListSize = 5

marginH : Int
marginH = 30

marginW : Int
marginW = 30

boxColor : Color
boxColor = rgba 150 90 240 0.8

pageColor : Color
pageColor = rgba 220 220 256 0.8 

-- ---------------------------------------
-- Intermission, featuring Markdown
--
--
mainHeader : Element
mainHeader = [markdown|

Let's make a Linked List!
=========================


|]
--
--
-- End of Markdown stuff
-- ---------------------------------------

-- Functions
main : Signal Element
main = lift2 page content.signal Window.dimensions 

content : Input Field.Content
content = input Field.noContent

page : Field.Content -> (Int, Int) -> Element
page fieldContent (w,h) = color pageColor <|
  container w h midTop (scene fieldContent (w-marginW, h-marginH))

scene : Field.Content -> (Int, Int) -> Element
scene fieldContent (w,h) = flow down
    [ mainHeader
    , color white <| Field.field Field.defaultStyle content.handle identity "..."
        fieldContent
    , collage w (w // defaultListSize) 
        (buildTree (w-50) (makeList fieldContent.string))
    ]
  

makeList : String -> [Element]
makeList ss =
  if Str.isEmpty ss
     then []
     else let ns = map (Maybe.maybe 100 (\n -> n)) <| map Str.toInt (Str.words ss)
          in rec 0 ns

rec : Int -> [Int] -> [Element]
rec i ns = 
  let mval = mHead <| drop i ns
      madr = mHead <| drop (i+1) ns
  in
     if Maybe.isJust madr
        then let val = Maybe.maybe -1 (\n -> n) mval
                 adr = Maybe.maybe -1 (\n -> n) madr
             in (plainText <| ((show i)++(":")++(show val)++(" -> ")++(show adr))) :: rec adr ns
        else []

mHead : [a] -> Maybe a
mHead bss = case bss of
  (b::bs) -> Just b
  []      -> Nothing


strToElems : String -> [Element]
strToElems ss =
  if Str.isEmpty ss
     then []
     else map plainText (Str.words ss)

buildTree : Int -> [Element] -> [Form]
buildTree w bs =
     let i = max defaultListSize (length bs)
         s = (w // i)
     in boxes (w-s) s i bs

boxes : Int -> Int -> Int -> [Element] -> [Form]
boxes w s i bss = case bss of
  (b::bs) ->
    let offset = ((toFloat w) / 2 ) - (toFloat ((i-1) * s))
    in (moveX offset (box s b)) :: (boxes w s (i-1) bs)
  [] -> []

box : Int -> Element -> Form 
box s b = 
  let sf = 0.7 * (toFloat s)
  in toForm (collage s s
              [ (filled boxColor (rect sf sf))
              , toForm b
              ])
