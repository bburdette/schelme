module SExpression exposing
    ( Sxp(..)
    , showSxp
    , sTerm
    , sList
    , sSxp
    , sSxps
    , spaces
    )

{-| Parse S-Expressions, yielding STerms and SLists

@docs Sxp
@docs showSxp
@docs sTerm
@docs sList
@docs sSxp
@docs sSxps
@docs spaces

-}

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


{-| S expression - either a string or a list of s expressions.
-}
type Sxp
    = STerm String
    | SList (List Sxp)


{-| print an s-expression for debug
-}
showSxp : Sxp -> String
showSxp sexp =
    case sexp of
        STerm s ->
            s

        SList ls ->
            "(" ++ String.concat (List.map showSxp ls) ++ ")"


{-| parse an individual s-expression term (not a list).
-}
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


{-| parse an s-expression - either a term or a list.
-}
sSxp : Parser Sxp
sSxp =
    oneOf
        [ sTerm
        , sList
        ]


{-| parse a series of space separated terms.
-}
sSxps : Parser (List Sxp)
sSxps =
    succeed (::)
        |= sSxp
        |= listOf
            (succeed identity
                |. spaces
                |= sSxp
            )


{-| a list is a series of space separated terms surrounded by parens.
-}
sList : Parser Sxp
sList =
    succeed SList
        |. symbol "("
        |= lazy (\_ -> sSxps)
        |. symbol ")"


{-| whitespace
-}
spaces : Parser ()
spaces =
    succeed ()
        |. oneOf [ symbol " ", symbol "\n", symbol "\t" ]
        |. chompWhile (\char -> char == ' ' || char == '\n' || char == '\t')
