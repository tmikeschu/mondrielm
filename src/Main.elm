module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Random



---- MODEL ----


type alias Model =
    { canvas : List (List Color)
    , height : Int
    , width : Int
    }


type alias Height =
    Int


type alias Width =
    Int


type CanvasElement
    = Box Height Width
    | VerticalRow
    | HorizontalRow


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


init : ( Model, Cmd Msg )
init =
    ( { canvas = []
      , height = 50
      , width = 50
      }
    , newCanvas 50 50
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewCanvas (List (List Color))
    | ChangeHeight Height
    | ChangeWidth Width
    | SetSquare


randomRow : Width -> Random.Generator (List Color)
randomRow w =
    Random.list w (Random.uniform Black [ Blue, Red, Yellow, White ])


randomCanvas : Height -> Width -> Random.Generator (List (List Color))
randomCanvas h w =
    Random.list h <| randomRow w


newCanvas : Height -> Width -> Cmd Msg
newCanvas h w =
    Random.generate NewCanvas (randomCanvas h w)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCanvas canvas ->
            ( { model | canvas = canvas }, Cmd.none )

        ChangeHeight h ->
            ( { model | height = model.height + h }, newCanvas (model.height + h) model.width )

        ChangeWidth w ->
            ( { model | width = model.width + w }, newCanvas model.height (model.width + w) )

        SetSquare ->
            let
                smaller =
                    min model.width model.height
            in
            ( { model | width = smaller, height = smaller }
            , newCanvas smaller smaller
            )

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
makeRow =
    List.map makeCell >> div [ class "Row" ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "MondriElm!" ]
        , div [ class "Adjustors" ]
            [ button [ onClick <| ChangeHeight 5 ] [ text "Height +" ]
            , button [ onClick <| ChangeHeight -5 ] [ text "Height -" ]
            , button [ onClick <| ChangeWidth 5 ] [ text "Width +" ]
            , button [ onClick <| ChangeWidth -5 ] [ text "Width -" ]
            , button [ onClick <| SetSquare ] [ text "Make Square" ]
            ]
        , div [ class "Frame" ] <| List.map makeRow model.canvas
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
