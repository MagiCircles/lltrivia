import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)

import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import String
import Task
import StartApp
import Html.Events exposing (onClick)

import Random
import Random exposing (Seed)

-- Misc functions

port randomSeed : Float

initialSeed : Seed
initialSeed = Random.initialSeed <| round randomSeed

randomInt : Int -> Seed -> (Int, Seed)
randomInt upper seed = (Random.generate (Random.int 0 upper) seed)

nth : Int -> List a -> Maybe a
nth n l =
  case l of
     [] -> Nothing
     xs::x -> if n <= 0 then Just xs else nth (n - 1) x

takeRandom : List a -> Seed -> Maybe (a, Seed)
takeRandom l seed =
  let len = List.length l - 1 in
  let (qid, s) = randomInt len seed in
  case nth qid l of
    Just n -> Just (n, s)
    Nothing -> Nothing

randomChoices : Idol -> Int -> List Idol -> Seed -> (List Idol, Seed)
randomChoices idol num idols seed =
  let aux idolss n acc seed =
        if n == 0 then (acc, seed) else
          case takeRandom idolss seed of
            Nothing -> (acc, seed)

            Just (choosen, seed) -> aux (List.filter ((/=) choosen) idolss)
                                    (n - 1) (choosen::acc) seed
  in
    aux (List.filter ((/=) idol) idols) num [idol] seed

shuffleList : List Idol -> Seed -> (List Idol, Seed)
shuffleList idols seed =
  let len = List.length idols in
  case idols of
    [] -> (idols, seed)
    x::[] -> (idols, seed)
    x::xs -> randomChoices x (len - 1) idols seed

-- Types and constants

url : String
url = "http://schoolido.lu/api/idols/?ordering=random&cards__is_special=False&page_size=100"

type alias Idol =
  { name : String
  , japanese_name : String
  , chibi : String
  , attribute : String
  , birthday : String
  , height : Int
  , favorite_food : Maybe String
  , least_favorite_food : Maybe String
  , hobbies : Maybe String
  }

type QuestionType
  = Food String
  | LeastFood String
  | Hobby String

type Question
  = Pending Idol (List Idol) QuestionType
  | End
  | Debug String

type Action
  = Restart
  | GotIdols (Maybe (List Idol))
  | Answer QuestionType Idol

type alias Model =
  { idols : Maybe (List Idol)
  , question : Question
  , seed : Seed
  , score : List Bool
  }

init : (Model, Effects Action)
init =
  ({ score = [],
     question = Debug "init",
     idols = Nothing,
     seed = initialSeed },
     getRandomIdol ())

-- Decoders

(**) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(**) func value = Json.object2 (<|) func value

idolDecoder : Json.Decoder (List Idol)
idolDecoder =
  let idol =
    Json.map Idol ("name" := Json.string)
          ** ("japanese_name" := Json.string)
          ** ("chibi_small" := Json.string)
          ** ("attribute" := Json.string)
          ** ("birthday" := Json.string)
          ** ("height" := Json.int)
          ** (Json.maybe ("favorite_food" := Json.string))
          ** (Json.maybe ("least_favorite_food" := Json.string))
          ** (Json.maybe ("hobbies" := Json.string))
  in
  "results" := Json.list idol

pickIdol : List Idol -> Seed -> Maybe (Idol, Seed)
pickIdol l = takeRandom l

mapQuestion : (String -> a) -> Maybe String -> List a -> List a
mapQuestion ctor element l =
  case element of
    Nothing -> l
    Just e -> (ctor e)::l

pickQuestion : Maybe (List Idol) -> Seed -> (Question, Seed)
pickQuestion idols seed =
  case idols of
    Just idols ->
      case pickIdol idols seed of
        Just (idol, seed) ->
          let questions = mapQuestion Food idol.favorite_food [] |>
                          mapQuestion LeastFood idol.least_favorite_food |>
                          mapQuestion Hobby idol.hobbies in
          case takeRandom questions seed of
            Just (question, seed) ->
              let (choices, seed) = randomChoices idol 3 idols seed in
              let (shuffled, seed) = shuffleList choices seed in
              (Pending idol shuffled question, seed)

            Nothing ->
              (Debug "Error while choosing a question", seed)

        Nothing ->
          (Debug "Error while picking an idol for this question", seed)

    Nothing ->
      (Debug "Error while fetching idols", seed)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Restart ->
      let (question, seed) = pickQuestion model.idols model.seed in
      ({question = question, seed = seed, score = [], idols = model.idols}, Effects.none)

    Answer question idol ->
      case model.question of
        Pending response _ _ ->
          let score = if response.name /= idol.name
                      then False
                      else True in
          let (question, seed) = if (List.length model.score) /= 10
                                 then pickQuestion model.idols model.seed
                                 else (End, model.seed) in
          ({model | question = question, seed = seed, score = (score::model.score)}, Effects.none)

        _ ->
          ({model | question = (Debug "Error")}, Effects.none)

    GotIdols i ->
      let (question, seed) = pickQuestion i model.seed in
      ({model | question = question, seed = seed, idols = i}, Effects.none)

-- Views related stuff

idolOptions : Signal.Address Action -> QuestionType -> Idol -> List Idol -> List Html
idolOptions address question idol idols =
  let format element =
        img [src element.chibi, onClick address (Answer question element)] [text element.name]
  in
  let aux acc l =
        case l of
          [] -> acc
          x::xs -> aux ((format x)::acc) xs
  in
    aux [] idols

formatQuestion : QuestionType -> Html
formatQuestion q =
  let s = case q of
            Hobby s ->
              "Who likes " ++ s ++ "?"

            Food s ->
              "Who likes " ++ s ++ "?"

            LeastFood s ->
              "Who dislikes " ++ s ++ "?"
  in
  text s

resultView : Signal.Address Action -> Model -> Html
resultView addr model =
  let score = List.foldl (\answer s -> if answer then s + 1 else s) 0 model.score in
  div
    []
      [text ("Your score is: " ++ toString score),
         button [onClick addr Restart] [text "try again"]
      ]


view : Signal.Address Action -> Model -> Html
view address model =
 let str = case model.question of
  Debug s -> text s

  End -> resultView address model

  Pending idol choices question ->
    let fquestion = formatQuestion question in
    case model.idols of
      Just idols -> div [] ([fquestion] ++ (idolOptions address question idol choices))

      Nothing -> text "lol"
 in
 div
 [ class "sit"]
 [ strong [] [str]]

-- Main and effects

getRandomIdol : () -> Effects Action
getRandomIdol str =
 Http.get idolDecoder url
    |> Task.toMaybe
    |> Task.map GotIdols
    |> Effects.task

app =
  StartApp.start
  {
     init = init
   , update = update
   , view = view
   , inputs = []
  }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
