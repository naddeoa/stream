module Main exposing (..)

import Html exposing (Html, div, text, input)
import Html.Attributes as Attributes
import Html.Events as Events
import Stream exposing (Stream)


type alias Model =
    { stream : Stream Int
    , results : List Int
    , subscriptionResults : List Int
    }


type Msg
    = Deferred (Stream.StreamResult Int)
    | Timer (Stream.StreamResult Int)
    | GetResult
    | GetResults


view : Model -> Html Msg
view { stream, results, subscriptionResults } =
    div []
        [ Html.button [ Events.onClick GetResults ] [ text "get next batch" ]
        , Html.button [ Events.onClick GetResult ] [ text "get next one" ]
        , Html.ul []
            (List.map (\n -> Html.li [] [ text <| toString n ]) results)
        , Html.br [] []
        , Html.ul []
            (List.map (\n -> Html.li [] [ text <| toString n ]) subscriptionResults)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetResults ->
            ( model, (Stream.deferNextN 1000 10 model.stream Deferred) )

        GetResult ->
            ( model, (Stream.deferNext 1000 model.stream Deferred) )

        Deferred results ->
            case results of
                Stream.Result nextStream result ->
                    ( { model
                        | stream = nextStream
                        , results = Maybe.withDefault [] <| Maybe.map List.singleton result
                      }
                    , Cmd.none
                    )

                Stream.Results nextStream results ->
                    ( { model | stream = nextStream, results = results }, Cmd.none )

        Timer results ->
            case results of
                Stream.Result nextStream result ->
                    ( { model
                        | stream = nextStream
                        , subscriptionResults = Maybe.withDefault [] <| Maybe.map List.singleton result
                      }
                    , Cmd.none
                    )

                Stream.Results nextStream results ->
                    ( { model | stream = nextStream, subscriptionResults = results }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Stream.every 2000 model.stream Timer


init : ( Model, Cmd Msg )
init =
    ( Model Stream.naturalNumbers [] [], Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }
