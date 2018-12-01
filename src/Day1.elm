module Day1 exposing (puzzle1, puzzle2)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Step, succeed)
import SantasLittleHelper exposing (..)
import Set exposing (Set)


puzzle1 input =
    Parser.run parseList input
        |> Result.map
            (\ok ->
                ok
                    |> List.sum
                    |> String.fromInt
            )
        |> Result.withDefault "Failed to parse."


parseValue =
    Parser.succeed (\a b -> ifThenElse a b -b)
        |= (Parser.chompIf
                (\a -> a == '+' || a == '-')
                |> Parser.getChompedString
                |> Parser.map (\a -> a == "+")
           )
        |= Parser.int


parseList : Parser (List Int)
parseList =
    Parser.loop [] parseListHelp


parseListHelp : List Int -> Parser (Step (List Int) (List Int))
parseListHelp revStmts =
    Parser.oneOf
        [ succeed (\stmt -> Parser.Loop (stmt :: revStmts))
            |= parseValue
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revStmts))
        ]


puzzle2 input =
    Parser.run parseList input
        |> Result.map
            (firstRepeat
                >> Result.map String.fromInt
                >> resultToString
            )
        |> Result.withDefault "Failed to parse."


resultToString : Result String String -> String
resultToString result =
    case result of
        Ok text ->
            text

        Err text ->
            text


firstRepeat : List Int -> Result String Int
firstRepeat values =
    firstRepeatHelper Set.empty 0 0 (Array.fromList values)


firstRepeatHelper : Set Int -> Int -> Int -> Array Int -> Result String Int
firstRepeatHelper seenValues currentValue index values =
    if Set.member currentValue seenValues then
        Ok currentValue

    else if index > 10000000 then
        Err "Exceeded max iterations."

    else if Array.isEmpty values then
        Err "No values."

    else
        let
            arrayIndex =
                Basics.modBy (Array.length values) index

            newValue =
                Array.get arrayIndex values |> Maybe.withDefault 0
        in
        firstRepeatHelper
            (Set.insert currentValue seenValues)
            (currentValue + newValue)
            (index + 1)
            values
