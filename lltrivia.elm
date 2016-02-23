import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)

import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Array
import Random.Array exposing (shuffle, choose, sample)
import String
import Task
import StartApp
import Html.Events exposing (onClick)

import Random
import Random exposing (Seed)

-- Misc functions

port randomSeed : Float
port btnColor : String

initialSeed : Seed
initialSeed = Random.initialSeed <| round randomSeed

nth : Int -> List a -> Maybe a
nth n l =
  case l of
     [] -> Nothing
     xs::x -> if n <= 0 then Just xs else nth (n - 1) x

randomChoices : Idol -> Int -> Array.Array Idol -> Seed -> (List Idol, Seed)
randomChoices idol num idols seed =
  let aux arr n acc seed =
        if n == 0 then (acc, seed) else
          let (choosen, seed, rest) = choose seed arr in
          case choosen of
            Nothing -> (acc, seed)
            Just choosen -> aux rest (n - 1) (choosen::acc) seed
  in
    aux idols num [idol] seed

shuffleList : List Idol -> Seed -> (List Idol, Seed)
shuffleList idols seed =
  let (shuffled, seed) = shuffle seed (Array.fromList idols) in
  (Array.toList shuffled, seed)

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

type IdolQuestionType
  = Food String Idol (List Idol)
  | LeastFood String Idol (List Idol)
  | Hobby String Idol (List Idol)

idolAnswer : IdolQuestionType -> Idol
idolAnswer question =
  case question of
    Food _ idol _ -> idol
    LeastFood _ idol _ -> idol
    Hobby _ idol _ -> idol

idolChoices : IdolQuestionType -> List Idol
idolChoices question =
  case question of
    Food _ _ idols -> idols
    LeastFood _ _ idols -> idols
    Hobby _ _ idols -> idols

type CardQuestionType
  = NotYet
  | Implemented

type Question
  = IdolQuestion IdolQuestionType
  | CardQuestion CardQuestionType

type State
  = Pending Question
  | End
  | Debug String
  | Init

type Action
  = Restart
  | GotIdols (Maybe (List Idol))
  | IdolAnswer Idol

type alias Model =
  { idols : Maybe (List Idol)
  , state : State
  , seed : Seed
  , score : List Bool
  }

init : (Model, Effects Action)
init =
  ({ score = [],
     state = Init,
     idols = Nothing,
     seed = initialSeed },
     getIdols ())

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

-- update

mapQuestion : (String -> a) -> Maybe String -> List a -> List a
mapQuestion ctor element l =
  case element of
    Nothing -> l
    Just e -> (ctor e)::l

pickQuestion : Maybe (List Idol) -> Seed -> (State, Seed)
pickQuestion idols seed =
  case idols of
    Just idols ->
      case choose seed (Array.fromList idols) of
        (Just idol, seed, idols) ->
          let questions = mapQuestion Food idol.favorite_food [] |>
                          mapQuestion LeastFood idol.least_favorite_food |>
                          mapQuestion Hobby idol.hobbies in
          case choose seed (Array.fromList questions) of
            (Just question, seed, _) ->
              let (choices, seed) = randomChoices idol 5 idols seed in
              let (shuffled, seed) = shuffleList choices seed in
              (Pending (IdolQuestion (question idol shuffled)), seed)

            (_, _, _) ->
              (Debug "Error while choosing a question", seed)

        (_, _, _)  ->
          (Debug "Error while picking an idol for this question", seed)

    Nothing ->
      (Debug "Error while fetching idols", seed)

checkIdolAnswer : IdolQuestionType -> Idol -> Bool
checkIdolAnswer question idol =
  let answer = idolAnswer question in
  idol.name == answer.name

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Restart ->
      let (question, seed) = pickQuestion model.idols model.seed in
      ({ state = question, seed = seed, score = [], idols = model.idols}, Effects.none)

    IdolAnswer idol ->
      case model.state of
        Pending (IdolQuestion question) ->
          let score = checkIdolAnswer question idol in
          let (question, seed) = if (List.length model.score) /= 9
                                 then pickQuestion model.idols model.seed
                                 else (End, model.seed) in
          ({model | state = question, seed = seed, score = (score::model.score)}, Effects.none)

        _ ->
          ({model | state = (Debug "Error")}, Effects.none)

    GotIdols i ->
      let (question, seed) = pickQuestion i model.seed in
      ({model | state = question, seed = seed, idols = i}, Effects.none)

-- Views related stuff

idolOptions : Signal.Address Action -> List Idol -> List Html
idolOptions address idols =
  let format element =
        figure
          [class "trivia_idol"
          ]
            [img
               [ src element.chibi
               , onClick address (IdolAnswer element)
               ] [text element.name]
            , figcaption
               []
               [ text element.name ]
            ]
  in
  let aux acc l =
        case l of
          [] -> acc
          x::xs -> aux ((format x)::acc) xs
  in
    aux [] idols

formatQuestion : IdolQuestionType -> Html
formatQuestion q =
  let s = case q of
            Hobby s _ _->
              "Who likes " ++ (String.toLower s) ++ "?"

            Food s _ _ ->
              "Who likes " ++ (String.toLower s) ++ "?"

            LeastFood s _ _ ->
              "Who dislikes " ++ (String.toLower s) ++ "?"
  in
    div
      [ class ("question text-" ++ btnColor) ]
      [text s]

formatProgress : List Bool -> Html
formatProgress l =
  div [ class "progress" ]
      (List.map (\b -> div
                   [ class ("progress-bar progress-bar-"
                              ++ (if b then "success" else "danger")) ] []) (List.reverse l))

formatComment : Int -> String
formatComment score =
  if (score <= 0) then "Ouch!"
  else if (score <= 3) then "Oh no..."
  else if (score <= 5) then "Meh."
  else if (score <= 7) then "Not bad!"
  else if (score <= 8) then "Yay~"
  else if score == 9 then "Awesome!"
  else "Woohoo!"

resultView : Signal.Address Action -> Model -> Html
resultView addr model =
  let score = List.foldl (\answer s -> if answer then s + 1 else s) 0 model.score in
  let fprogress = formatProgress model.score in
  let comment = formatComment score in
  div [ class "final_result" ]
      [ div
         [ class "row"]
         [ div [ class "col-md-6" ]
             [ h3 [] [ text "Final Score" ]
             , span [ class "final_score" ] [ text (toString score) ]
             ]
         , div [ class "col-md-6" ]
             [ p [ class "score_comment" ]
                [ text comment ]
             , p [ class "text-right" ]
                 [a [ href ("http://twitter.com/share?text=" ++ (toString score) ++ "/10 on School Idol Trivia! " ++ comment ++ " Play with me:&via=schoolidolu&url=http://schoolido.lu/trivia/&hashtags=LLSIF,LoveLive,スクフェス")
                    , target "_blank"
                    , class "btn btn-Cool"
                    ]
                    [ img [src "/static/twitter.png"] []
                    , text " Tweet your score"
                    ]
                 , br [] [], br [] []
                 , Html.form [ method "POST"
                        , action "/ajax/trivia/share/"
                        ]
                    [ input [ type' "hidden"
                            , name "score"
                            , value (toString score) ]
                        []
                    , input [ type' "submit"
                            , value "Share on School Idol Tomodachi"
                            , class "btn btn-link"
                            ]
                        []
                    ]
                 , br [] [], br [] []
                 , a [ onClick addr Restart
                    , href "#"
                    ] [text "Try again"]
                 , br [] [], br [] []
                 ]
             ]
         ]
         , fprogress
      ]


view : Signal.Address Action -> Model -> Html
view address model =
 let str = case model.state of
  Debug s -> text s

  End -> resultView address model

  Pending question ->
    let content =
          case question of
            IdolQuestion question ->
              let fquestion = formatQuestion question in
              let foptions = idolOptions address (idolChoices question) in
              [fquestion] ++ foptions
            _ -> []
    in
    let fprogress = formatProgress model.score in
    case model.idols of
      Just idols -> div [] (content ++
                              [fprogress])

      Nothing -> text "Weird"

  Init ->
      i
        [ class "flaticon-loading" ]
        []
 in
 div
 [ class "sit"]
 [ strong [] [str]]

-- Main and effects

getIdols : () -> Effects Action
getIdols _ =
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
