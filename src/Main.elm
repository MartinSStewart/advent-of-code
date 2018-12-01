module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List.Extra as List
import Puzzle1


type alias Model =
    { inputs : Dict Int String }


type Msg
    = InputChanged Int String


initialModel : Model
initialModel =
    { inputs = Dict.empty }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged index input ->
            { model | inputs = model.inputs |> Dict.insert index input }


view : Model -> Html Msg
view model =
    let
        puzzles =
            [ Puzzle1.solution ]
                |> List.indexedMap
                    (\index solution ->
                        puzzleView
                            index
                            (Dict.get index model.inputs |> Maybe.withDefault "")
                            solution
                    )
    in
    div [] puzzles


puzzleView : Int -> String -> (String -> String) -> Html Msg
puzzleView index input puzzleSolution =
    let
        puzzle =
            Basics.modBy 2 index |> (+) 1 |> String.fromInt

        day =
            index // 2 |> (+) 1 |> String.fromInt

        title =
            "Dec " ++ day ++ " - Puzzle " ++ puzzle
    in
    div []
        [ Html.h3 [] [ text title ]
        , div [] [ text "Input" ]
        , Html.textarea [ Html.Events.onInput (InputChanged index) ] []
        , div [] [ text "Output" ]
        , Html.textarea
            [ Html.Attributes.readonly True
            , input |> puzzleSolution |> Html.Attributes.value
            ]
            []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
