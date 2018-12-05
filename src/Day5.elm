module Day5 exposing (puzzle1)

import List.Extra as List


puzzle1 : String -> String
puzzle1 input =
    puzzle1Helper input |> String.length |> String.fromInt


puzzle1Helper : String -> String
puzzle1Helper input =
    let
        letterPass : List Char
        letterPass =
            input
                |> String.toList
                |> List.foldr
                    (\letter state ->
                        let
                            invertedCase : Char
                            invertedCase =
                                if Char.isLower letter then
                                    Char.toUpper letter

                                else
                                    Char.toLower letter
                        in
                        if Just invertedCase == List.head state then
                            List.tail state |> Maybe.withDefault []

                        else
                            letter :: state
                    )
                    []

        newString : String
        newString =
            String.fromList letterPass
    in
    if newString == input then
        input

    else
        puzzle1Helper newString
