module View where

import Color exposing (..)
import Graphics.Element exposing (..)
import Text


main =
  let toyScreen = screen 1 32 48 in
  let phoneScreen = screen 10 320 480 in
  let landscapeScreen = screen 10 480 320 in
  let padScreen = screen 10 768 1024 in
  let sampleCell = cell 34 lightYellow "25" in
  let sampleTable x = table x <| \_ -> \_ -> sampleCell in
  let largeTable x = table x <| \_ -> \_ -> cell 68 lightYellow "25" in
  flow down <| List.map (test >> indented 10)
    [ ("Screen border simulation", toyScreen empty)
    , ("Table 3 &times; 3", sampleTable 3 |> phoneScreen)
    , ("Table 5 &times; 5", sampleTable 5 |> phoneScreen)
    , ("Table 7 &times; 7", sampleTable 7 |> phoneScreen)
    , ("Table 3 &times; 3", sampleTable 3 |> landscapeScreen)
    , ("Table 5 &times; 5", sampleTable 5 |> landscapeScreen)
    , ("Table 7 &times; 7", sampleTable 7 |> landscapeScreen)
    , ("Table 7 &times; 7", largeTable 7 |> padScreen)
    ]


table : Int -> (Int -> Int -> Element) -> Element
table size render =
  List.map (render >> row size) [1..size] |>
    flow down

row : Int -> (Int -> Element) -> Element
row size render =
  List.map render [1..size] |>
    flow right


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
