module Sukutomo where

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

type alias Card =
  { name : String
  , japanese_name : String
  , rarity : String
  , attribute : String
  , card_image : Maybe String
  , transparent_image : Maybe String
  , card_idolized_image : Maybe String
  , transparent_idolized_image : Maybe String
  }

type alias Song =
  { name : String
  , romaji_name : Maybe String
  , translated_name : Maybe String
  , attribute : String
  , image : String
  , itunes_id : Maybe Int
  }
