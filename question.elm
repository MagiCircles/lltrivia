module Question where

import Sukutomo exposing (Idol, Card, Song)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Json.Decode

port btnColor : String

type Quizz
  = Idols
  | Cards
  | Songs
  | All

type Action
  = Restart
  | GotIdols (Maybe (List Idol))
  | GotRandomCards (Maybe (List Card))
  | GotSongs (Maybe (List Song))
  | Answer Question
  | SongInfoReceived Json.Decode.Value
  | ChangeQuizz Quizz

type alias Question =
  {  question : QuestionKind
  ,  answer : Maybe AnswerKind }

type QuestionKind
  = IdolFood String Idol (List Idol)
  | IdolLeastFood String Idol (List Idol)
  | IdolHobby String Idol (List Idol)
  | SongPlay (Maybe String) Int Song (List Song)
  | SongCover String Song (List Song)
  | CardAttribute String Card
  | CardRarity String Card
  | CardDetail String Idol (List Idol)

type AnswerKind
  = IdolAnswer Idol
  | StringAnswer String
  | SongAnswer Song

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

        SongCover _ song _ ->
          case answer of
            SongAnswer s -> song.name == s.name
            _ -> False

        SongPlay _ _ song _ ->
          case answer of
            SongAnswer s -> song.name == s.name
            _ -> False

        CardAttribute _ card ->
          case answer of
            StringAnswer s -> s == card.attribute
            _ -> False

        CardRarity _ card ->
          case answer of
            StringAnswer s -> s == card.rarity
            _ -> False

        CardDetail _ idol _ ->
          case answer of
            IdolAnswer i -> i.name == idol.name
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

            SongPlay surl s _ _->
              let formatItunesURL id =
                    "https://itunes.apple.com/lookup?id=" ++ toString s ++ "&callback=elm.ports.songInfo.send" in
              let content =
                    case surl of
                      Nothing ->
                        node "script" [ src (formatItunesURL s) ] []
                      Just url ->
                        audio [controls True] [source [src url, type' "audio/mp4"] []  ] in
                    div [ class "row" ] [
                     div [ class "col-md-7" ] [
                            span [ class "song-question" ] [ text "What is this song?" ]
                           ]
                    , div [ class "col-md-5" ] [
                             content
                            ] ]

            SongCover s _ _ ->
              div [ class "row" ] [
                     div [ class "col-md-7" ] [
                            span [ class "question-with-image" ] [ text "What is this song?" ]
                           ]
                    , div [ class "col-md-5" ] [
                             div [ class "song-image-question-detail"
                                 , style [ ("background-image", "url('" ++ s ++ "')") ]
                                 ] []
                            ]
                    ]

            CardAttribute transparent _ ->
              div [ class "row" ] [
                     div [ class "col-md-7" ] [
                            span [ class "question-with-image" ] [ text "What is the attribute of this card?" ]
                           ]
                    , div [ class "col-md-5" ] [
                             img [ class "image-with-question"
                                 , src transparent] []
                            ]
                    ]

            CardRarity transparent _ ->
              div [ class "row" ] [
                     div [ class "col-md-7" ] [
                            span [ class "question-with-image" ] [ text "What is the rarity of this card?" ]
                           ]
                    , div [ class "col-md-5" ] [
                             img [ class "image-with-question"
                                 , src transparent] []
                            ]
                    ]

            CardDetail image _ _ ->
              div [ class "row" ] [
                     div [ class "col-md-7" ] [
                            span [ class "question-with-image" ] [ text "Who is the idol in this card?" ]
                           ]
                    , div [ class "col-md-5" ] [
                             div [ class "image-question-detail"
                                 , style [ ("background-image", "url('" ++ image ++ "')") ]
                                 ] []
                            ]
                    ]

  in
    div
      [ class ("question text-" ++ btnColor) ]
      [s]

stringOptions : Signal.Address Action -> Question -> List String -> List Html
stringOptions address question strings =
  let format str =
        a [href "#", onClick address (Answer <| (answerQuestion (StringAnswer str) question))] [text str] in
  List.map format strings

imageOptions : Signal.Address Action -> Question -> List (String, String) -> List Html
imageOptions address question images =
  let format (image, str) =
        a [ href "#", onClick address (Answer <| (answerQuestion (StringAnswer str) question)) ] [
             img [ src image, alt str, class "choice-image" ] []
            ] in
  List.map format images

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

songOptions : Signal.Address Action -> Question -> List Song -> List Html
songOptions address question songs =
  let format element =
        case question.question of
          SongPlay _ song _ _ ->
            figure
              [class "trivia_song"
              ]
              [img
                 [ src element.image
                 , onClick address (Answer <| (answerQuestion (SongAnswer element) question))
                 ] [text element.name]
              , figcaption
                 []
                 [ text element.name ]
              ]
          SongCover song _ _ ->
            div
              [class "trivia_song"
              ]
              [span
                 [ onClick address (Answer <| (answerQuestion (SongAnswer element) question))
                 ] [text element.name]
              ]

          _ -> div [] []


  in
  let aux acc l =
        case l of
          [] -> acc
          x::xs -> aux ((format x)::acc) xs
  in
    aux [] songs

optionsToHtml : Signal.Address Action -> Question -> List Html
optionsToHtml address question =
  case question.question of
    IdolHobby _ _ idols -> idolOptions address question idols

    IdolFood _ _ idols -> idolOptions address question idols

    IdolLeastFood _ _ idols -> idolOptions address question idols

    SongCover _ _ songs -> songOptions address question songs

    SongPlay _ _ _ songs -> songOptions address question songs

    CardRarity _ _ -> imageOptions address question
                       [ ("http://i.schoolido.lu/static/N" ++ btnColor ++ ".png", "N")
                       , ("http://i.schoolido.lu/static/R" ++ btnColor ++ ".png", "R")
                       , ("http://i.schoolido.lu/static/SR" ++ btnColor ++ ".png", "SR")
                       , ("http://i.schoolido.lu/static/UR" ++ btnColor ++ ".png", "UR")
                       ]

    CardAttribute _ _ -> imageOptions address question
                         [ ("http://i.schoolido.lu/static/Smile.png", "Smile")
                         , ("http://i.schoolido.lu/static/Pure.png", "Pure")
                         , ("http://i.schoolido.lu/static/Cool.png", "Cool")
                         , ("http://i.schoolido.lu/static/All.png", "All")
                         ]

    CardDetail _ _ idols -> idolOptions address question idols
