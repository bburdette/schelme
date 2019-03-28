module SExpression exposing (Sxp(..), sList, sSxp, sSxps, sTerm, showSxp, spaces)

import ParseHelp exposing (listOf)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , andThen
        , chompIf
        , chompWhile
        , float
        , getChompedString
        , keyword
        , lazy
        , loop
        , map
        , oneOf
        , run
        , sequence
        , succeed
        , symbol
        )
import Util exposing (first, rest)


{-| S expression
-}
type Sxp
    = STerm String
    | SList (List Sxp)


showSxp : Sxp -> String
showSxp sexp =
    case sexp of
        STerm s ->
            s

        SList ls ->
            "(" ++ String.concat (List.map showSxp ls) ++ ")"


sTerm : Parser Sxp
sTerm =
    let
        stermchar =
            \c ->
                (c /= '\'')
                    && (c /= ' ')
                    && (c /= '\n')
                    && (c /= '\t')
                    && (c /= '(')
                    && (c /= ')')
    in
    succeed STerm
        |= (getChompedString <|
                succeed ()
                    |. chompIf stermchar
                    |. chompWhile stermchar
           )


sSxp : Parser Sxp
sSxp =
    oneOf
        [ sTerm
        , sList
        ]


sSxps : Parser (List Sxp)
sSxps =
    succeed (::)
        |= sSxp
        |= listOf
            (succeed identity
                |. spaces
                |= sSxp
            )


sList : Parser Sxp
sList =
    succeed SList
        |. symbol "("
        |= lazy (\_ -> sSxps)
        |. symbol ")"


spaces : Parser ()
spaces =
    succeed ()
        |. oneOf [ symbol " ", symbol "\n", symbol "\t" ]
        |. chompWhile (\char -> char == ' ' || char == '\n' || char == '\t')
