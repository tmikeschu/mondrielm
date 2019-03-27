module Tests exposing (all)

import Canvas exposing (withHorizontalBorders, withVerticalBorders)
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
        ]
