
module ReverseArray where

{-| Implement a function to reverse an array. -}

import Signal as S
import Graphics.Input as I
import Graphics.Input.Field as F
import Graphics.Element as E
import Keyboard as K
import Char

import Set
import String
import List
import Debug
import Array as A
import Window



type Config = {debug : Bool}

config : Config
config = {debug = True}

textInput = I.input F.noContent
textField = F.field F.defaultStyle textInput.handle identity ""

click = I.input ()
reverseButton = image 30 30 "/exercises/glyphicons_173_play.png" |> I.clickable click.handle ()

parseString : String -> A.Array Char
parseString = String.toList >> A.fromList

reverseArray : A.Array a -> A.Array a
reverseArray array = reverseArray' 0 (A.length array - 1) array

reverseArray' : Int -> Int -> A.Array a -> A.Array a
reverseArray' start end array =
  let finished = start >= end
      reverseArray'' startVal endVal array = A.set end startVal array
        |> A.set start endVal
        |> reverseArray' (start + 1) (end - 1)
  in
      if finished then array else reverseArray'' (A.getOrFail start array) (A.getOrFail end array) array

debugReverseArray array = Debug.watch "pre reverse" array |> reverseArray >> Debug.watch "post reverse"

logReverseArray config =
  if config.debug then debugReverseArray else reverseArray

arrayAsText array = A.toList array |> asText

clearOnClick = S.sampleOn click.signal <| S.constant F.noContent
clearOnEnter = S.sampleOn K.enter <| S.constant F.noContent
textFieldContent = S.merges [clearOnEnter, clearOnClick, textInput.signal]
reverseOn = S.sampleOn (S.merge clearOnEnter clearOnClick) (arrayAsText << reverseArray << parseString << .string <~ textInput.signal)

desc =
  [markdown|
  ### Reverse the typed characters "in place".

  This is deliberately made difficult in order to simulate a  \
  reversal of an array as if this were implemented in C.  \
  In other words, we ignore all the nice things Elm gives us.
  |]

scene content reversedContent (w, h) =
  let x = absolute 10
      y = absolute 5
      pos = topLeftAt x y
  in flow down [desc, textField content `beside` E.spacer 5 5 `beside` reverseButton, reversedContent] |> container w h pos

main = scene <~ textFieldContent ~ reverseOn ~ Window.dimensions
--main = scene <~ textFieldContent ~ S.constant "abcd" ~ Window.dimensions
--main = A.initialize 6 (\n -> n*n) |> (logReverseArray config >> arrayAsText)
--main = A.initialize 6 (\n -> n*n) |> (debugReverseArray >> arrayAsText)
--main = A.initialize 6 (\n -> n*n) |> Debug.watch "pre reverse" |> reverseArray |> Debug.watch "post reverse" |> A.toList |> asText

--main = toText "code" |> (monospace >> leftAligned)
--main = leftAligned << monospace <| toText "code"

--main = textField <~ (uniqueSignal (debugContent <~ textInput.signal))
--main = uniqueChars "test" |> show |> asText
--main = uniqueContent (F.Content "test" (F.Selection 0 0 F.Forward)) |> show |> asText
--main = F.Content "tes" (F.Selection 0 0 F.Forward) |> uniqueContent |> show |> asText

