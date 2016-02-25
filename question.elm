module Question where

import Sukutomo exposing (Idol, Card)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String

type Quizz
  = Idols
  | Cards
  | All

type Action
  = Restart
  | GotIdols (Maybe (List Idol))
  | GotRandomCard (Maybe Card)
  | Answer Question
  | ChangeQuizz Quizz

type alias Question =
  {  question : QuestionKind
  ,  answer : Maybe AnswerKind }

type QuestionKind
  = IdolFood String Idol (List Idol)
  | IdolLeastFood String Idol (List Idol)
  | IdolHobby String Idol (List Idol)
  | CardAttribute String Card
  | CardRarity String Card

type AnswerKind
  = IdolAnswer Idol
  | StringAnswer String

newQuestion : QuestionKind -> Question
newQuestion question =
  { question = question, answer = Nothing }

answerQuestion : AnswerKind -> Question -> Question
answerQuestion answer question =
  { question = question.question, answer = Just answer }

checkAnswer : Question -> Bool
checkAnswer question =
  case question.answer of
    Nothing -> True

    Just answer ->
      case question.question of
        IdolFood _ idol _ ->
          case answer of
            IdolAnswer i -> idol.favorite_food == i.favorite_food
            _ -> False

        IdolLeastFood _ idol _ ->
          case answer of
            IdolAnswer i -> idol.least_favorite_food == i.least_favorite_food
            _ -> False

        IdolHobby _ idol _ ->
          case answer of
            IdolAnswer i -> idol.hobbies == i.hobbies
            _ -> False

        CardAttribute attribute _ ->
          case answer of
            StringAnswer s -> s == attribute
            _ -> False

        CardRarity rarity _ ->
          case answer of
            StringAnswer s -> s == rarity
            _ -> False

questionToHtml : Question -> String -> Html
questionToHtml question btnColor =
  let s = case question.question of
            IdolHobby s _ _->
              text <| "Who likes " ++ (String.toLower s) ++ "?"

            IdolFood s _ _ ->
              text <| "Who likes " ++ (String.toLower s) ++ "?"

            IdolLeastFood s _ _ ->
              text <| "Who dislikes " ++ (String.toLower s) ++ "?"

            CardAttribute transparent _ ->
              div [] [text "attribute", img [src transparent] []]

            CardRarity transparent _ ->
              div [] [text "rarity", img [src transparent] []]
  in
    div
      [ class ("question text-" ++ btnColor) ]
      [s]

stringOptions : Signal.Address Action -> Question -> List String -> List Html
stringOptions address question strings =
  let format str =
        a [href "#", onClick address (Answer <| (answerQuestion (StringAnswer str) question))] [text str] in
  List.map format strings

idolOptions : Signal.Address Action -> Question -> List Idol -> List Html
idolOptions address question idols =
  let format element =
        figure
          [class "trivia_idol"
          ]
            [img
               [ src element.chibi
               , onClick address (Answer <| (answerQuestion (IdolAnswer element) question))
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

optionsToHtml : Signal.Address Action -> Question -> List Html
optionsToHtml address question =
  case question.question of
    IdolHobby _ _ idols -> idolOptions address question idols

    IdolFood _ _ idols -> idolOptions address question idols

    IdolLeastFood _ _ idols -> idolOptions address question idols

    CardAttribute _ _ -> stringOptions address question ["UR", "SR", "R", "N"]

    CardRarity _ _ -> stringOptions address question ["Smile", "Cool", "Pure", "All"]
