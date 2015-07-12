module View where

import Color exposing (..)
import Graphics.Element exposing (..)
import Text


main =
  let toyScreen = screen 1 32 48 in
  let phoneScreen = screen 10 320 480 in
  let landscapeScreen = screen 10 480 320 in
  let padScreen = screen 10 768 1024 in
  let mcell s c x = toString x |> cell s c in
  let sampleCell = mcell 34 in
  let yellowCell = sampleCell lightYellow in
  let blueCell = sampleCell lightBlue in
  let checkered = List.indexedMap <| \i -> if i % 2 == 0 then blueCell else yellowCell in
  let sampleTable x = grid x (checkered [1..(x*x)]) in
  flow down <| List.map (test >> indented 10)
    [ ("Screen border simulation", toyScreen empty)
    , ("Grid", grid 3 (List.map show [1..9]))
    , ("Table 3 &times; 3", sampleTable 3 |> phoneScreen)
    , ("Table 5 &times; 5", sampleTable 5 |> phoneScreen)
    , ("Table 7 &times; 7", sampleTable 7 |> phoneScreen)
    , ("Table 3 &times; 3", sampleTable 3 |> landscapeScreen)
    , ("Table 5 &times; 5", sampleTable 5 |> landscapeScreen)
    , ("Table 7 &times; 7", sampleTable 7 |> landscapeScreen)
    ]


grid : Int -> (List Element) -> Element
grid width els =
  case els of
    [] -> empty
    otherwise ->
      flow right (List.take width els)
      `above`
      grid width (List.drop width els)

cell : Int -> Color -> String -> Element
cell size color' text =
  let percent p x = (x * p) // 100 in
  let size' = percent 105 size in
  centered (Text.fromString text) |>
    container size' size' middle <<
    color color' <<
    container size size middle


indented : Int -> Element -> Element
indented indentation element =
  flow right
    [ spacer indentation 1
    , element
    ]


screen : Int -> Int -> Int -> Element -> Element
screen border width height element =
  let w' = width + 2 * border in
  let h' = height + 2 * border in
    element |>
      color black <<
      container w' h' middle <<
      color white <<
      container width height middle


test : (String, Element) -> Element
test (description, element) =
  flow down
    [ spacer 1 32
    , centered <|
        Text.fromString description
    , element
    ]
