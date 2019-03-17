module Schelm exposing (Sxp(..), sList, sTerm, sTerms, showSxp, spaces)

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
import TDict exposing (TDict)
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


sTerm : Parser String
sTerm =
    succeed identity
        |= (getChompedString <|
                succeed ()
                    |. chompWhile
                        (\c ->
                            (c /= '\'')
                                && (c /= ' ')
                                && (c /= '(')
                                && (c /= ')')
                        )
           )


sTerms : Parser (List String)
sTerms =
    succeed (::)
        |= sTerm
        |= listOf
            (succeed identity
                |. spaces
                |= sTerm
            )


sList : Parser (List String)
sList =
    succeed identity
        |. symbol "("
        |= sTerms
        |. symbol ")"


spaces : Parser ()
spaces =
    succeed ()
        |. symbol " "
        |. chompWhile (\char -> char == ' ')
