module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Random



---- Step 1: random square ----
---- MODEL ----
---- TODO: whimsical vim ----
-- random hex value
-- takes dimensions of square and the atomic dimension (one square)


type alias Model =
    List (List Color)


type Color
    = Blue
    | Red
    | Yellow
    | Black
    | White


colorToString : Color -> String
colorToString c =
    case c of
        Blue ->
            "#3963BA"

        Red ->
            "#B41907"

        Yellow ->
            "#EDB023"

        Black ->
            "#000000"

        White ->
            "#ffffff"



-- start 10 by 10, atomic dimension 1rem


init : ( Model, Cmd Msg )
init =
    ( []
    , newCanvas
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewCanvas (List (List Color))


randomRow : Random.Generator (List Color)
randomRow =
    Random.list 10 (Random.uniform Black [ Blue, Red, Yellow, White ])


randomCanvas : Random.Generator (List (List Color))
randomCanvas =
    Random.list 10 randomRow


newCanvas : Cmd Msg
newCanvas =
    Random.generate NewCanvas randomCanvas


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCanvas rows ->
            ( rows, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ---


makeCell : Color -> Html Msg
makeCell c =
    div
        [ class "Cell"
        , style "background-color" (colorToString c)
        , style "width" "1rem"
        , style
            "height"
            "1rem"
        ]
        []


makeRow : List Color -> Html Msg
makeRow r =
    div [ class "Row" ] <| List.map makeCell r


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mondrian!" ]
        , div [ class "Frame" ] <| List.map makeRow model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
