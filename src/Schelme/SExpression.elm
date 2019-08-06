module Schelme.SExpression exposing
    ( Sxp(..)
    , showSxp
    , sTerm
    , sList
    , sSxp
    , sSxps
    )

{-| Parse S-Expressions, yielding STerms and SLists

@docs Sxp
@docs showSxp
@docs sTerm
@docs sList
@docs sSxp
@docs sSxps

-}

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , chompIf
        , chompUntil
        , chompWhile
        , getChompedString
        , lazy
        , map
        , oneOf
        , succeed
        , symbol
        )
import Schelme.ParseHelp exposing (listOf)


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
    oneOf
        [ nonquotedString
        , quotedString
        ]


nonquotedString : Parser Sxp
nonquotedString =
    let
        stermchar =
            \c ->
                (c /= '\'')
                    && (c /= ' ')
                    && (c /= '"')
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


quotedString : Parser Sxp
quotedString =
    succeed STerm
        |= (getChompedString <|
                succeed ()
                    |. chompIf ((==) '"')
                    |. chompUntil "\""
                    |. chompIf (always True)
           )


{-| parse an s-expression - either a term or a list.
-}
sSxp : Parser Sxp
sSxp =
    oneOf
        [ sTerm
        , sList
        ]


{-| parse a series of whitespace separated terms.
-}
sSxps : Parser (List Sxp)
sSxps =
    succeed (::)
        |. Parser.spaces
        |= sSxp
        |= listOf
            (succeed identity
                |. Parser.backtrackable rqspaces
                |= sSxp
            )
        |. Parser.spaces


{-| a list is a series of space separated terms surrounded by parens.
-}
sList : Parser Sxp
sList =
    succeed SList
        |. symbol "("
        |= lazy (\_ -> sSxps)
        |. symbol ")"


{-| required whitespace
-}
rqspaces : Parser ()
rqspaces =
    succeed ()
        |. oneOf [ symbol " ", symbol "\n", symbol "\t" ]
        |. chompWhile (\char -> char == ' ' || char == '\n' || char == '\t')
