module Util exposing (Size, deadEndToString, deadEndsToString, first, maxInt, mbPList, mblist, minInt, paramParser, paramsParser, problemToString, rest, rslist, trueforany)

import Dict exposing (Dict)
import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=), Problem(..), symbol)


type alias Size =
    { width : Int
    , height : Int
    }


maxInt : Int
maxInt =
    9007199254740991


minInt : Int
minInt =
    -9007199254740991


paramParser : P.Parser ( String, String )
paramParser =
    P.succeed (\a b -> ( a, b ))
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '='))
        |. P.symbol "="
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '&'))


paramsParser : P.Parser (Dict String String)
paramsParser =
    P.succeed (\a b -> Dict.fromList <| a :: b)
        |= paramParser
        |= listOf
            (P.succeed identity
                |. symbol "&"
                |= paramParser
            )


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


mbPList : List a -> List b -> Maybe (List ( a, b ))
mbPList aees bees =
    case ( List.head aees, List.head bees ) of
        ( Just a, Just b ) ->
            Maybe.map ((::) ( a, b )) (mbPList (rest aees) (rest bees))

        ( Nothing, Nothing ) ->
            Just []

        _ ->
            Nothing


first : (a -> Maybe b) -> List a -> Maybe b
first f l =
    case List.head l of
        Just e ->
            case f e of
                Just x ->
                    Just x

                Nothing ->
                    first f (rest l)

        Nothing ->
            Nothing


trueforany : (a -> Bool) -> List a -> Bool
trueforany f l =
    case List.head l of
        Just e ->
            if f e then
                True

            else
                trueforany f (rest l)

        Nothing ->
            False


mblist : List (Maybe a) -> Maybe (List a)
mblist mbs =
    Maybe.map List.reverse <|
        List.foldl
            (\mba mblst ->
                case mblst of
                    Nothing ->
                        Nothing

                    Just lst ->
                        case mba of
                            Nothing ->
                                Nothing

                            Just a ->
                                Just <| a :: lst
            )
            (Just [])
            mbs


rslist : List (Result a b) -> Result a (List b)
rslist rslts =
    Result.map List.reverse <|
        List.foldl
            (\mba rslst ->
                case rslst of
                    Err e ->
                        Err e

                    Ok lst ->
                        case mba of
                            Err e ->
                                Err e

                            Ok a ->
                                Ok <| a :: lst
            )
            (Ok [])
            rslts


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : P.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : P.Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting octal"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting number"

        ExpectingNumber ->
            "expecting variable"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
