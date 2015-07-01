import Graphics.Element exposing (Element, show)
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

update : (Time, Action) -> State -> State
update (time, action) state =
  case action of

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

input = Signal.constant (StartGame 3)

main = Signal.map view <| Signal.foldp update Greetings (Time.timestamp input)

view : State -> Element
view state =
  case state of
    Greetings -> show "Greetings"
    StartedAt _ game -> show game
    Lose game -> show "Loser"
    Finished time -> show time

mistake : Game -> UserGuess -> Bool
mistake game userGuess =
  case  whereIs game.guess game.table of
    Just answer -> userGuess == answer
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
