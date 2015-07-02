import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Random
import Time exposing (Time)

type alias Number = Int
type alias Table = List (List Number)
type alias Size = Int
type alias UserGuess = Size

type alias Game =
  { table : Table
  , guess : Number
  }

type State = Greetings
           | StartedAt Time Game
           | Lose Game
           | Finished Time

type Action = StartGame Size
            | TapColumn UserGuess
            | None

update : (Time, Action) -> State -> State
update (time, action) state =
  case action of
    None -> state

    StartGame size ->
      StartedAt time { table = randomTable size time
                     , guess = 1
                     }

    TapColumn userGuess ->
      case state of
        StartedAt startTime game ->
          if mistake game userGuess then
            Lose game
          else let size = List.length game.table in
               let last = size * size in
            if game.guess == last then
              Finished (time - startTime)
            else
              StartedAt startTime { game | guess <- game.guess + 1 }

        otherwise -> Greetings

input : Signal.Mailbox Action
input = Signal.mailbox None

main = Signal.map view <| Signal.foldp update Greetings (Time.timestamp input.signal)

view : State -> Element
view state =
  let address = Signal.message input.address in
  case state of
    Greetings ->
      flow down [ button (address (StartGame 3)) "3 &times; 3"
                , button (address (StartGame 5)) "5 &times; 5"
                , button (address (StartGame 7)) "7 &times; 7"
                ]

    StartedAt _ game ->
      flow down [ showTable plainColor game.table
                , flow right
                    <| List.map (\x -> button (address (TapColumn x)) (toString x)) [1..(List.length game.table)]
                ]

    Lose game ->
      flow down [ showTable (highlightWrong game.guess) game.table
                , show "Loser"
                ]

    Finished time -> show time

plainColor : Number -> Element
plainColor = genericColor <| \_ -> lightYellow

highlightWrong : Number -> Number -> Element
highlightWrong highlight =
  genericColor <| \x -> if x == highlight then
                          lightRed
                        else
                          lightYellow

genericColor : (Number -> Color) -> Number -> Element
genericColor colorize number =
  container 45 45 middle
  << color (colorize number)
  << container 40 40 middle
  <| show number

showTable : (Number -> Element) -> Table -> Element
showTable decorate table =
  let column = showColumn decorate in
  flow right
    <| List.map column table

showColumn : (Number -> Element) -> List Number -> Element
showColumn decorate column =
  flow down
    <| List.map decorate column

mistake : Game -> UserGuess -> Bool
mistake game userGuess =
  case whereIs game.guess game.table of
    Just answer -> userGuess /= answer
    Nothing -> True

whereIs : Number -> Table -> Maybe UserGuess
whereIs number table =
  case table of
    column :: rest ->
      if List.member number column then
        Just 1
      else
        whereIs number rest `Maybe.andThen` \x -> Just (x + 1)

    [] -> Nothing

randomTable : Size -> Time -> Table
randomTable size time =
  let numbers = randomList (size * size) time in
    split size numbers

randomList : Size -> Time -> List Number
randomList size time =
  if size > 0 then
    shuffle [1..size] time
  else
    []

shuffle : List Number -> Time -> List Number
shuffle list time =
     List.map snd
  <| List.sort
  <| List.map2 (,) (random (List.length list) time) list

random : Size -> Time -> List Float
random size time =
  let seed = Random.initialSeed (round time) in
    fst <| Random.generate (Random.list size (Random.float 0 1)) seed

split : Size -> List a -> List (List a)
split size list =
  case list of
    [] -> []
    list -> [List.take size list] ++ (split size (List.drop size list))
