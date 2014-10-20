module UniqueCharacters where

{-| Implement an algorithm to determine if a string has all unique characters. What
if you cannot use additional data structures?
-}

import Signal as S
import Graphics.Input as I
import Graphics.Input.Field as F
import Graphics.Element as E
import Set
import String
import List
import Debug



textInput = I.input F.noContent

textField = F.field F.defaultStyle textInput.handle identity ""

uniqueContent : F.Content -> Bool
uniqueContent content = uniqueChars content.string

uniqueChars : String -> Bool
uniqueChars str =
  let set = String.foldr (\c s -> Set.insert c s) Set.empty str
      strLength = String.length str
      setLength = Set.toList set |> List.length
  in
      strLength == setLength

uniqueSignal : Signal F.Content -> Signal F.Content
uniqueSignal signal = keepIf uniqueContent F.noContent signal

desc =
  [markdown|
  ### A field that only displays unique characters typed
  |]

scene content = container 500 100 middle <| flow down [desc, textField content]
main = scene <~ uniqueSignal textInput.signal


debugContent : F.Content -> F.Content
debugContent c = Debug.watchSummary "uniqueContent" .string c

--main = textField <~ (uniqueSignal (debugContent <~ textInput.signal))
--main = uniqueChars "test" |> show |> asText
--main = uniqueContent (F.Content "test" (F.Selection 0 0 F.Forward)) |> show |> asText
--main = F.Content "tes" (F.Selection 0 0 F.Forward) |> uniqueContent |> show |> asText

