module Main exposing
    ( Direction(..)
    , Model
    , Msg(..)
    , NetworkData(..)
    , RateEl
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
    , ratesToSelectView
    , subscriptions
    , update
    , view
    )

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, dict, field, float, int, string)
import Task



{- TYPES -}


type Direction
    = From
    | To


type alias RateEl =
    ( Currency, Factor )



{- MODEL -}


type alias Model =
    { networkData : NetworkData
    , rateFrom : String
    , rateTo : String
    , valueFrom : Float
    , valueTo : Float
    }


type NetworkData
    = Loading
    | Success Recieved
    | Failure Http.Error


type alias Recieved =
    { rates : Rates
    , base : Currency
    }


type alias Rates =
    Dict Currency Factor


type alias Currency =
    String


type alias Factor =
    Float



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
converter { valueTo, valueFrom } { rates } =
    div []
        [ Html.h1 [] [ text (directionToString From) ]
        , ratesToSelectView From rates
        , Html.input
            [ Html.Events.onInput (UpdateInput From rates)
            , Attrs.value <| String.fromFloat <| valueFrom
            ]
            []
        , Html.h1 [] [ text (directionToString To) ]
        , ratesToSelectView To rates
        , Html.input
            [ Html.Events.onInput (UpdateInput To rates)
            , Attrs.value <| String.fromFloat <| valueTo
            ]
            []
        ]


ratesToSelectView : Direction -> Rates -> Html Msg
ratesToSelectView direction rates =
    Html.select
        [ Html.Events.onInput (SetComparable direction)
        ]
        (rates
            |> Dict.toList
            |> List.map optionFromRateView
        )


optionFromRateView : RateEl -> Html Msg
optionFromRateView rate =
    let
        value =
            rate |> Tuple.first
    in
    Html.option
        [ Attrs.value value ]
        [ text value ]


computeValueTo : Rates -> Model -> Float -> Float
computeValueTo rates model val =
    -- TODO:
    let
        fromFactor =
            factorByCurrency rates model.rateFrom

        toFactor =
            factorByCurrency rates model.rateTo

        value =
            Just val
    in
    Maybe.map2
        (/)
        value
        fromFactor
        |> Maybe.map2 (*) toFactor
        |> Maybe.withDefault 0


factorByCurrency : Rates -> Currency -> Maybe Factor
factorByCurrency rates currency =
    rates |> Dict.get currency



{- UPDATE -}


type Msg
    = GotRates (Result Http.Error Recieved)
    | Refresh
    | SetComparable Direction String
    | UpdateInput Direction Rates String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRates result ->
            case result of
                Ok val ->
                    ( { model
                        | networkData = Success val
                        , rateFrom = val.rates |> Dict.keys |> List.head |> Maybe.withDefault ""
                        , rateTo = val.rates |> Dict.keys |> List.head |> Maybe.withDefault ""
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | networkData = Failure err }, Cmd.none )

        Refresh ->
            ( { model | networkData = Loading }, getRates )

        UpdateInput dir rates val ->
            let
                value =
                    computeValueTo rates model (stringToFloat val)
            in
            case dir of
                From ->
                    ( { model
                        | valueFrom = value
                        , valueTo = value
                      }
                    , Cmd.none
                    )

                To ->
                    ( { model
                        | valueTo = value
                        , valueFrom = value
                      }
                    , Cmd.none
                    )

        SetComparable dir str ->
            case dir of
                From ->
                    ( { model | rateFrom = str }, Cmd.none )

                To ->
                    ( { model | rateTo = str }, Cmd.none )


stringToFloat : String -> Float
stringToFloat str =
    case String.toFloat str of
        Just val ->
            val

        Nothing ->
            0



{- DECODERS -}


getRawRates : Decoder Rates
getRawRates =
    JD.field "rates" <| dict float


extractRatesList : Decoder Recieved
extractRatesList =
    JD.map2 Recieved
        getRawRates
        parseBase


parseBase : Decoder Currency
parseBase =
    field "base" string


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
    , rateFrom = ""
    , rateTo = ""
    , valueFrom = 0
    , valueTo = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getRates )



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
