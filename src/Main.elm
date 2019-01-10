module Hello exposing (..)

import Browser exposing (element)
import Http
import Html exposing  (Html, text, div, button)
import Html.Events exposing (onClick)
import Debug
import Json.Decode exposing (Decoder, field, float)

-- MAIN

main = element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- INIT

init: () -> (Model, Cmd Msg)
init _ = 
    (Loading, getRates)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

type Model 
  = Loading
  | Success Float
  | Failure

view: Model -> Html Msg
view model = 
  case model of 
    Failure ->
      div [] [text "Error"]
    Success float -> 
      div [] [
        div [] [text (Debug.toString float)]
        , button [onClick Refresh ] [ text "Refresh"]
      ]
    Loading ->
      text "Loading"

type Msg 
  = GotRates (Result Http.Error Float)
  | Refresh

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRates result ->
      case result of
        Ok float->  
          (Success float, Cmd.none)
        Err _ -> 
          (Failure, Cmd.none)
    Refresh ->
      (Loading, getRates)

-- HELPERS

ratesDecoder: Decoder Float
ratesDecoder = 
  field "rates" (field "PLN" float)


getRates: Cmd Msg
getRates = 
  Http.get 
    {
      url = "https://api.exchangeratesapi.io/latest"
      , expect = Http.expectJson GotRates ratesDecoder
    }
