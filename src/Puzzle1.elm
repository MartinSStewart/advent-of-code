module Puzzle1 exposing (solution)

import Parser exposing ((|.), (|=), Parser, Step, succeed)
import SantasLittleHelper exposing (..)


solution input =
    Parser.run parseList input
        |> Result.map
            (\ok ->
                ok
                    |> List.map (\{ isPositive, value } -> ifThenElse isPositive value -value)
                    |> List.sum
                    |> String.fromInt
            )
        |> Result.withDefault "Failed to parse."


type alias Value =
    { isPositive : Bool, value : Int }


parseValue =
    Parser.succeed Value
        |= (Parser.chompIf
                (\a -> a == '+' || a == '-')
                |> Parser.getChompedString
                |> Parser.map (\a -> a == "+")
           )
        |= Parser.int


parseList : Parser (List Value)
parseList =
    Parser.loop [] parseListHelp


parseListHelp : List Value -> Parser (Step (List Value) (List Value))
parseListHelp revStmts =
    Parser.oneOf
        [ succeed (\stmt -> Parser.Loop (stmt :: revStmts))
            |= parseValue
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revStmts))
        ]
