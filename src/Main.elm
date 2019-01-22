module Main exposing
    ( Currency(..)
    , Direction(..)
    , Model
    , Msg(..)
    , NetworkData(..)
    , Rate
    , Rates
    , Recieved
    , converter
    , directionToString
    , extractRatesList
    , getRates
    , getRawRates
    , init
    , initialModel
    , main
    , parseBase
    , ratesSelect
    , subscriptions
    , update
    , view
    )

import Browser
import Debug
import Html exposing (Html, button, div, text)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, field, float, int, string)



{- TYPES -}


type alias Rates =
    List Rate


type alias Rate =
    ( String, Float )


type Currency
    = Currency String


type Val
    = Val String


type alias Recieved =
    { rates : Rates
    , base : Currency
    }


type alias Model =
    { networkData : NetworkData
    , rateFrom : Maybe String
    , rateTo : Maybe String
    , fromValue : Maybe Float
    , toValue : Maybe Float
    }


type NetworkData
    = Loading
    | Success Recieved
    | Failure Http.Error


type Direction
    = From
    | To



{- MAIN -}


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{- INITIAL -}


initialModel : Model
initialModel =
    { networkData = Loading
    , rateFrom = Nothing
    , rateTo = Nothing
    , fromValue = Nothing
    , toValue = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getRates )



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{- VIEW -}


view : Model -> Html Msg
view model =
    case model.networkData of
        Loading ->
            div [] [ text "loading..." ]

        Success rates ->
            converter model rates

        Failure err ->
            div [] [ text <| Debug.toString err ]


converter : Model -> Recieved -> Html Msg
converter model recieved =
    div []
        [ Html.h1 [] [ text (directionToString From) ]
        , ratesSelect From recieved.rates
        , Html.input [ Html.Events.onInput OnUpdateFrom ] []
        , Html.h1 [] [ text (directionToString To) ]
        , ratesSelect To recieved.rates
        , Html.input [ Attrs.value <| Debug.toString model.fromValue ] []
        ]


ratesSelect : Direction -> Rates -> Html Msg
ratesSelect direction rates =
    Html.select
        [ Html.Events.onInput (SetComparable direction)
        ]
        (List.map
            optionFromRate
            rates
        )


optionFromRate : Rate -> Html Msg
optionFromRate rate =
    let
        value =
            Tuple.first rate
    in
    Html.option
        [ Attrs.value value ]
        [ text value ]



{- UPDATE -}


type Msg
    = GotRates (Result Http.Error Recieved)
    | Refresh
    | SetComparable Direction String
    | OnUpdateFrom String
    | OnUpdateTo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRates result ->
            case result of
                Ok val ->
                    ( { model | networkData = Success val }, Cmd.none )

                Err err ->
                    ( { model | networkData = Failure err }, Cmd.none )

        Refresh ->
            ( { model | networkData = Loading }, getRates )

        OnUpdateFrom val ->
            ( { model | fromValue = String.toFloat val }, Cmd.none )

        OnUpdateTo val ->
            ( { model | toValue = String.toFloat val }, Cmd.none )

        SetComparable dir str ->
            case dir of
                From ->
                    ( { model | rateFrom = Just str }, Cmd.none )

                To ->
                    ( { model | rateTo = Just str }, Cmd.none )



{- DECODERS -}


getRawRates : Decoder Rates
getRawRates =
    JD.field "rates" (JD.keyValuePairs JD.float)


extractRatesList : Decoder Recieved
extractRatesList =
    JD.map2 Recieved
        getRawRates
        parseBase


parseBase =
    JD.map
        Currency
        (field "base" string)


getRates : Cmd Msg
getRates =
    Http.get
        { url = "https://api.exchangeratesapi.io/latest"
        , expect = Http.expectJson GotRates extractRatesList
        }


directionToString : Direction -> String
directionToString dir =
    case dir of
        From ->
            "From"

        To ->
            "To"
