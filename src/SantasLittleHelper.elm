module SantasLittleHelper exposing (ifThenElse, parseList, parseListHelp)

import Parser exposing ((|.), (|=), Parser)


ifThenElse : Bool -> a -> a -> a
ifThenElse condition ifTrue ifFalse =
    if condition then
        ifTrue

    else
        ifFalse


parseList : Parser a -> Parser (List a)
parseList parser =
    Parser.loop [] (parseListHelp parser)


parseListHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
parseListHelp parser revStmts =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Parser.Loop (stmt :: revStmts))
            |= parser
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revStmts))
        ]
