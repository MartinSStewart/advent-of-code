module ElmjutsuDumMyM0DuL3 exposing (..)
-- exposing (puzzle1, puzzle2)

import List.Extra as List
import SantasLittleHelper


puzzle1 input =
    let
        elementsWith2Or3Repeats : List ( Bool, Bool )
        elementsWith2Or3Repeats =
            String.words input
                |> List.map
                    (\a ->
                        let
                            groups =
                                a
                                    |> String.toList
                                    |> List.gatherEquals
                                    |> List.map (\( head, rest ) -> List.length rest + 1)

                            hasTwoOfLetter =
                                List.any ((==) 2) groups

                            hasThreeOfLetter =
                                List.any ((==) 3) groups
                        in
                        ( hasTwoOfLetter, hasThreeOfLetter )
                    )

        total2LetterRepeats : Int
        total2LetterRepeats =
            List.count Tuple.first elementsWith2Or3Repeats

        total3LetterRepeats : Int
        total3LetterRepeats =
            List.count Tuple.second elementsWith2Or3Repeats
    in
    String.fromInt (total2LetterRepeats * total3LetterRepeats)


puzzle2 input =
    let
        matchingPair : Maybe ( String, String )
        matchingPair =
            String.words input
                |> List.foldl
                    (\newValue ( previousValues, solution ) ->
                        let
                            newSolution : Maybe ( String, String )
                            newSolution =
                                if solution == Nothing then
                                    let
                                        match =
                                            List.find (haveOneDifference newValue) previousValues
                                    in
                                    match |> Maybe.map (\a -> ( newValue, a ))

                                else
                                    solution
                        in
                        ( newValue :: previousValues, newSolution )
                    )
                    ( [], Nothing )
                |> Tuple.second
    in
    matchingPair
        |> Maybe.map (\( a, b ) -> removeNonMatchingChars a b)
        |> Maybe.withDefault "No match found."


removeNonMatchingChars : String -> String -> String
removeNonMatchingChars first second =
    List.zip (String.toList first) (String.toList second)
        |> List.filter (\( a, b ) -> a == b)
        |> List.map Tuple.first
        |> String.fromList


haveOneDifference : String -> String -> Bool
haveOneDifference first second =
    List.zip (String.toList first) (String.toList second)
        |> List.count (\( a, b ) -> a /= b)
        |> (==) 1
