module Currency exposing (Currency(..), Data, Model(..), Msg(..), currencyToString, getRates, init, main, rateEnum, ratesDecoder, subscriptions, update, view)

import Browser exposing (element)
import Debug
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, float)



-- MAIN


main =
    element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRates )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


type Model
    = Loading
    | Success Float
    | Failure


type alias Data =
    { base : Maybe Currency
    , from : Maybe Currency
    , to : Maybe Currency
    }


type Currency
    = Currency String


rateEnum : List Currency
rateEnum =
    [ Currency "HRK"
    , Currency "HUF"
    , Currency "IDR"
    , Currency "PHP"
    , Currency "TRY"
    , Currency "RON"
    , Currency "ISK"
    , Currency "SEK"
    , Currency "THB"
    , Currency "PLN"
    , Currency "GBP"
    , Currency "CAD"
    , Currency "AUD"
    , Currency "MYR"
    , Currency "NZD"
    , Currency "CHF"
    , Currency "DKK"
    , Currency "SGD"
    , Currency "CNY"
    , Currency "BGN"
    , Currency "CZK"
    , Currency "BRL"
    , Currency "JPY"
    , Currency "KRW"
    , Currency "INR"
    , Currency "MXN"
    , Currency "RUB"
    , Currency "HKD"
    , Currency "USD"
    , Currency "ZAR"
    , Currency "ILS"
    , Currency "NOK"
    ]


currencyToString : Currency -> String
currencyToString currency =
    case currency of
        Currency string ->
            string


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div [] [ text "Error" ]

        Success float ->
            div []
                [ div [] [ text (Debug.toString float) ]
                , button [ onClick Refresh ] [ text "Refresh" ]
                ]

        Loading ->
            text "Loading"


type Msg
    = GotRates (Result Http.Error Float)
    | Refresh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRates result ->
            case result of
                Ok float ->
                    ( Success float, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        Refresh ->
            ( Loading, getRates )



-- HELPERS


ratesDecoder : Decoder Float
ratesDecoder =
    field "rates" (field "PLN" float)


getRates : Cmd Msg
getRates =
    Http.get
        { url = "https://api.exchangeratesapi.io/latest"
        , expect = Http.expectJson GotRates ratesDecoder
        }
