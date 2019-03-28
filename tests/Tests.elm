module Tests exposing (all)

import Canvas
    exposing
        ( getNeighbors
        , withBox
        , withHorizontalBorders
        , withVerticalBorders
        )
import Color exposing (..)
import Expect
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    let
        canvas =
            [ [ Red, White, Yellow ]
            , [ White, White, Red ]
            , [ Blue, White, Black ]
            ]
    in
    describe "Canvas"
        [ describe "withHorizontalBorders"
            [ test "transforms the specified row to the specified color" <|
                \_ ->
                    let
                        actual =
                            withHorizontalBorders [ 1 ] Black canvas

                        expected =
                            [ [ Red, White, Yellow ]
                            , [ Black, Black, Black ]
                            , [ Blue, White, Black ]
                            ]
                    in
                    Expect.equal actual expected
            , test "transforms multiple rows" <|
                \_ ->
                    let
                        actual =
                            withHorizontalBorders [ 0, 2 ] Black canvas

                        expected =
                            [ [ Black, Black, Black ]
                            , [ White, White, Red ]
                            , [ Black, Black, Black ]
                            ]
                    in
                    Expect.equal actual expected
            , test "ignores invalid row indices" <|
                \_ ->
                    let
                        actual =
                            withHorizontalBorders [ 0, 7, 8 ] Black canvas

                        expected =
                            [ [ Black, Black, Black ]
                            , [ White, White, Red ]
                            , [ Blue, White, Black ]
                            ]
                    in
                    Expect.equal actual expected
            ]
        , describe "withVerticalBorders"
            [ test "transforms the specified column to the specified color" <|
                \_ ->
                    let
                        actual =
                            withVerticalBorders [ 1 ] Black canvas

                        expected =
                            [ [ Red, Black, Yellow ]
                            , [ White, Black, Red ]
                            , [ Blue, Black, Black ]
                            ]
                    in
                    Expect.equal actual expected
            ]
        , describe "getNeighbors"
            [ test "takes a dimension and coordinates and returns a list of\n                coordinates" <|
                \_ ->
                    let
                        actual =
                            getNeighbors { height = 2, width = 2 } ( 2, 0 )

                        expected =
                            [ ( 2, 0 )
                            , ( 2, 1 )
                            , ( 3, 0 )
                            , ( 3, 1 )
                            ]
                    in
                    Expect.equal (List.sort actual) (List.sort expected)
            ]
        , describe "withBox"
            [ test "adds a box of specified dimensions and color to the canvas" <|
                \_ ->
                    let
                        canvas2 =
                            [ [ Yellow, Red, Black, White ]
                            , [ White, Blue, Black, White ]
                            , [ White, White, Black, Yellow ]
                            , [ Red, Blue, Black, Yellow ]
                            ]

                        actual =
                            withBox { height = 2, width = 2 } ( 2, 0 ) Red canvas2

                        expected =
                            [ [ Yellow, Red, Black, White ]
                            , [ White, Blue, Black, White ]
                            , [ Red, Red, Black, Yellow ]
                            , [ Red, Red, Black, Yellow ]
                            ]
                    in
                    Expect.equal actual expected
            ]
        ]
