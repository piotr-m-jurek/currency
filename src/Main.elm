module Hello exposing (..)

import Browser exposing (element)
import Http
import Html exposing  (Html, text)
import Debug
import Json.Decode exposing (Decoder, field, float)

main = element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init: () -> (Model, Cmd Msg)
init _ = 
    (Loading, getRates)

view: Model -> Html Msg
view model = 
  case model of 
    Failure ->
      text "Error"
    Success float -> 
      text (Debug.toString float)
    Loading ->
      text "Loading"

type Model 
  = Loading
  | Success Float
  | Failure

type Msg = GotRates (Result Http.Error Float)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRates result ->
      case result of
        Ok float->  
          (Success float, Cmd.none)
        Err _ -> 
          (Failure, Cmd.none)

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
