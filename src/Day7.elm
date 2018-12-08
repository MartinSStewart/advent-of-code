module Day7 exposing (puzzle1)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import SantasLittleHelper exposing (..)


puzzle1 input =
    case Parser.run (parseList rowParser) input of
        Ok dependencies ->
            dependencies
                |> List.map (\a -> [ a.before, a.after ])
                |> List.concat
                |> List.unique
                |> List.sortWith
                    (\a b -> LT
                     -- List.find (\dep -> dep.before == a || dep.after == a || dep.before == b || dep.after == b) dependencies
                     --     |> Maybe.withDefault '_'
                    )
                |> String.fromList

        Err error ->
            "Failed to parse."


type alias Dependency =
    { before : Char, after : Char }


rowParser : Parser Dependency
rowParser =
    Parser.succeed Dependency
        |. Parser.symbol "Step "
        |= chompChar
        |. Parser.symbol " must be finished before step "
        |= chompChar
        |. Parser.symbol " can begin."


chompChar : Parser Char
chompChar =
    Parser.chompIf (always True) |> Parser.getChompedString |> Parser.map (String.toList >> List.head >> Maybe.withDefault 'A')
