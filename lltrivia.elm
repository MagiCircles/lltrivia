import Sukutomo exposing (Idol, Card)
import Question
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Signal exposing (Signal, Address)

import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Array
import Random.Array exposing (shuffle, choose, sample)
import String
import Task
import StartApp

import Random
import Random exposing (Seed)

-- Misc functions

port randomSeed : Float
port btnColor : String
port cardTotal : Int

type alias Action = Question.Action

initialSeed : Seed
initialSeed = Random.initialSeed <| round randomSeed

randomChoices : Idol -> Int -> Array.Array Idol -> Seed -> (List Idol, Seed)
randomChoices idol num idols seed =
  let aux arr n acc seed =
        if n == 0 then (acc, seed) else
          let (choosen, seed', rest) = choose seed arr in
          case choosen of
            Nothing -> (acc, seed')
            Just choosen -> aux rest (n - 1) (choosen::acc) seed'
  in
    aux idols num [idol] seed

shuffleList : List a -> Seed -> (List a, Seed)
shuffleList idols seed =
  let (shuffled, seed') = shuffle seed (Array.fromList idols) in
  (Array.toList shuffled, seed')

getRandomIds : Int -> Seed -> (List Int, Seed)
getRandomIds n s =
  let aux n s acc =
      case n of
        0 -> (acc, s)
        n -> let gen = Random.int 1 cardTotal in
             let (rand_int, s) = Random.generate gen s in
             let acc = rand_int::acc in
             aux (n - 1) s acc in
  aux n s []

dropMaybe : List (Maybe a) -> List a
dropMaybe l =
  let aux l acc =
      case l of
        [] -> acc
        x::xs ->
          case x of
            Just a -> aux xs (a::acc)
            Nothing -> aux xs acc in
  aux l []

-- Types and constants

api_url : String
api_url = "http://schoolido.lu/api/"

idols_url : String
idols_url = api_url ++ "idols/?for_trivia=True&page_size=100"

random_cards_url : List Int -> String
random_cards_url ids =
  let aux ids url =
      case ids of
        [] -> String.dropRight 1 url
        x::xs -> aux xs (url ++ (toString x) ++ ",") in
  aux ids (api_url ++ "cards/?ids=")

type State
  = Pending Question.Question
  | End
  | NetworkFailure
  | Debug String
  | Init

type alias Model =
  { idols : Maybe (List Idol)
  , quizz : Question.Quizz
  , state : State
  , cards : Maybe (List Card)
  , seed : Seed
  , score : List Bool
  , retry : Int
  }

init : (Model, Effects Action)
init =
  ({ score = []
   , retry = 3
   , quizz = Question.All
   , state = Init
   , idols = Nothing
   , cards = Nothing
   , seed = initialSeed },
     getIdols ())

-- Decoders

(**) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(**) func value = Json.object2 (<|) func value

idolDecoder : Json.Decoder (List (Maybe Idol))
idolDecoder =
  let idol =
    Json.maybe (Json.map Idol ("name" := Json.string)
          ** ("japanese_name" := Json.string)
          ** ("chibi_small" := Json.string)
          ** ("attribute" := Json.string)
          ** ("birthday" := Json.string)
          ** ("height" := Json.int)
          ** (Json.maybe ("favorite_food" := Json.string))
          ** (Json.maybe ("least_favorite_food" := Json.string))
          ** (Json.maybe ("hobbies" := Json.string)))
  in
  "results" := Json.list idol

cardsDecoder : Json.Decoder (List (Maybe Card))
cardsDecoder =
  let at_idol = Json.at ["idol"] in
  let card =
    Json.maybe ( Json.map Card (at_idol ("name" := Json.string))
          ** (at_idol ("japanese_name" := Json.string))
          ** ("rarity" := Json.string)
          ** ("attribute" := Json.string)
          ** (Json.maybe ("card_image" := Json.string))
          ** (Json.maybe ("transparent_image" := Json.string))
          ** (Json.maybe ("card_idolized_image" := Json.string))
          ** (Json.maybe ("transparent_idolized_image" := Json.string)))
  in
  "results" := Json.list card

-- update

mapQuestion : (String -> a) -> Maybe String -> List a -> List a
mapQuestion ctor element l =
  case element of
    Nothing -> l
    Just e -> (ctor e)::l

mapMaybe : List (Maybe a, Maybe b) -> List (a, b)
mapMaybe l =
  let aux l acc =
    case l of
      (Just x, Just y)::xs -> aux xs ((x, y)::acc)
      _::xs -> aux xs acc
      [] -> acc
  in aux l []

getIdolAndOptions : Seed -> String -> List Idol -> (Maybe (Idol, List Idol), Seed)
getIdolAndOptions seed name idols =
  let aux idols acc idol =
      case idols of
        [] -> (idol, acc)
        x::xs ->
          if x.name == name then
            aux xs acc (Just x)
          else
            aux xs (x::acc) idol
  in
    case aux idols [] Nothing of
      (Just idol, l) ->
        let (choices, seed') = randomChoices idol 5 (Array.fromList l) seed in
        let (shuffled, seed'') = shuffleList choices seed' in
        (Just (idol, shuffled), seed'')
      (Nothing, _) ->
        (Nothing, seed)


pickCardQuestion : Card -> Seed -> List Idol -> (State, Seed)
pickCardQuestion card seed idols =
  let (images, seed') =
        sample seed (Array.fromList <|
                       mapMaybe [(card.card_image, card.transparent_image),
                                   (card.card_idolized_image, card.transparent_idolized_image)]) in
  case images of
    Just (card_image, transparent_image) ->
      case getIdolAndOptions seed' card.name idols of
        (Just (idol, idols), seed'') ->
              let questions = [ Question.CardAttribute transparent_image card
                              , Question.CardRarity transparent_image card
                              , Question.CardDetail card_image idol idols
                              ]
              in
                case sample seed'' (Array.fromList questions) of
                  (Just question, seed''') ->
                    (Pending <| Question.newQuestion question, seed''')

                  (Nothing, seed''') ->
                    (Debug "Error while picking question", seed''')

        (Nothing, seed'') ->
          (Debug "Error while finding the correct idol", seed'')

    Nothing ->
      (Debug "Error while picking idol type", seed')

pickIdolQuestion : List Idol -> Seed -> (State, Seed)
pickIdolQuestion idols seed =
  case choose seed (Array.fromList idols) of
    (Just idol, seed', idols) ->
      let questions = mapQuestion Question.IdolFood idol.favorite_food [] |>
                      mapQuestion Question.IdolLeastFood idol.least_favorite_food |>
                      mapQuestion Question.IdolHobby idol.hobbies in
      case choose seed' (Array.fromList questions) of
        (Just question, seed'', _) ->
          let (choices, seed''') = randomChoices idol 5 idols seed'' in
          let (shuffled, seed'''') = shuffleList choices seed''' in
          (Pending <| Question.newQuestion (question idol shuffled), seed'''')

        (_, _, _) ->
          (Debug "Error while choosing a question", seed)

    (_, _, _)  ->
          (Debug "Error while picking an idol for this question", seed)

pickQuestion : Question.Quizz -> Model -> (Model, Effects Action)
pickQuestion quizz model =
  if model.retry < 1 then
    ({ model | state = NetworkFailure }, Effects.none) else
  case model.idols of
    Nothing -> ({ model | state = Init }, getIdols ())
    Just idols ->
      case quizz of
        Question.Idols ->
          let (state, seed) = pickIdolQuestion idols model.seed in
          let model = { model | state = state, seed = seed } in
          (model, Effects.none)

        Question.Cards ->
          case model.cards of
            Nothing ->
              let (ids, seed) = getRandomIds 1 model.seed in
              ({ model | state = Init, seed = seed }, getCards ids)
            Just [] ->
              let (ids, seed) = getRandomIds 1 model.seed in
              ({ model | state = Init, seed = seed }, getCards ids)
            Just (card::cards) ->
              let (state, seed) = pickCardQuestion card model.seed idols in
              let model = { model | state = state, seed = seed, cards = (Just cards) } in
              (model, Effects.none)

        Question.All ->
          let choices = Array.fromList [Question.Cards, Question.Idols] in
          let (choice, seed) = sample model.seed choices in
          let model = { model | seed = seed } in
          case choice of
            Just quizz -> pickQuestion quizz model
            Nothing -> ({ model | state = Debug "Error while picking quizz" }, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Question.Restart ->
      let model = { model | score = [] } in
      pickQuestion model.quizz model

    Question.ChangeQuizz quizz ->
      let model = { model | score = [], quizz = quizz } in
      pickQuestion model.quizz model

    Question.Answer answer ->
      let score = Question.checkAnswer answer in
      let model = { model | score = (score::model.score) } in
      if (List.length model.score) /= 10
      then
        pickQuestion model.quizz model
      else
        let model = { model | state = End } in
        (model, Effects.none)

    Question.GotIdols i ->
      let model =
            case i of
              Just idols ->
                let (idols, seed) = shuffleList idols model.seed in
                { model | idols = (Just idols), seed = seed }
              Nothing ->
                if model.retry >= 1 then
                  { model | idols = i, retry = (model.retry - 1) }
                else
                  { model | state = NetworkFailure }
      in
      pickQuestion model.quizz model

    Question.GotRandomCards cards ->
      let model' =
            case cards of
              Just cards ->
                let (cards, seed) = shuffleList cards model.seed in
                { model | cards = (Just cards), seed = seed }
              Nothing ->
                if model.retry >= 1 then
                  { model | cards = cards, retry = (model.retry - 1) }
                else
                  { model | state = NetworkFailure }
      in
      pickQuestion model'.quizz model'

-- Views related stuff

formatQuizzButtons : Signal.Address Action -> Question.Quizz -> Html
formatQuizzButtons addr quizz =
  ul [ class "nav nav-tabs" ]
       [
        li (case quizz of
              Question.All -> [ class "active" ]
              _ -> []) [
              a [ href "#all", onClick addr (Question.ChangeQuizz Question.All) ] [
                   i [ class "flaticon-album" ] []
                  , text " All"
                  ]
             ]
       , li (case quizz of
              Question.Cards -> [ class "active" ]
              _ -> []) [
              a [ href "#cards", onClick addr (Question.ChangeQuizz Question.Cards) ] [
                   i [ class "flaticon-cards" ] []
                  , text " Cards"
                  ] ]
       , li (case quizz of
              Question.Idols -> [ class "active" ]
              _ -> []) [
              a [ href "#idols", onClick addr (Question.ChangeQuizz Question.Idols) ] [
                   i [ class "flaticon-idolized" ] []
                  , text " Idols"
                  ] ]
       ]


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
  let quizzbuttons = formatQuizzButtons addr model.quizz in
  let score = List.foldl (\answer s -> if answer then s + 1 else s) 0 model.score in
  let fprogress = formatProgress model.score in
  let comment = formatComment score in
  div [ class "final_result" ]
      [
       quizzbuttons,
       div
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
                 , a [ onClick addr Question.Restart
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

  NetworkFailure -> text "Network or Server problem, please refresh"

  End -> resultView address model

  Pending question ->
    let quizzbuttons = formatQuizzButtons address model.quizz in
    let fquestion = Question.questionToHtml question btnColor in
    let foptions = Question.optionsToHtml address question in
    let fprogress = formatProgress model.score in
    div [] <| [quizzbuttons, fquestion] ++ foptions ++ [fprogress]
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
 Http.get idolDecoder idols_url
    |> Task.map dropMaybe
    |> Task.toMaybe
    |> Task.map Question.GotIdols
    |> Effects.task

getCards : (List Int) -> Effects Action
getCards ids =
  Http.get cardsDecoder (random_cards_url ids)
    |> Task.map dropMaybe
    |> Task.toMaybe
    |> Task.map Question.GotRandomCards
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
