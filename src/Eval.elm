module Eval exposing (Function, NameSpace, Term(..), eval, evalFtn, parseNumber, parseString, parseSymbol, sxpToTerm, termString)

import Dict exposing (Dict)
import ParseHelp exposing (listOf)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Problem
        , Step(..)
        , andThen
        , chompIf
        , chompWhile
        , end
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
import SExpression exposing (Sxp(..))
import TDict exposing (TDict)
import Util exposing (first, rest)



{-
   "blah"

   2213.123512

   blah

   (defn (plus a b)
     (+ a b))

   (def blah 5)

   (plus blah blah)
-}


type alias Function =
    { args : List String, body : List Term }


type Term
    = TString String
    | TNumber Float
    | TList (List Term)
    | TSymbol String
    | TFunction Function
    | TBuiltIn (List Term -> NameSpace -> Result String ( NameSpace, Term ))


type alias NameSpace =
    Dict String Term


evalFtn : Function -> List Term -> NameSpace -> Result String ( NameSpace, Term )
evalFtn fn terms ns =
    case Util.mbPList fn.args terms of
        Nothing ->
            Err "number of args and terms don't match!"

        Just pl ->
            let
                fnns =
                    List.foldr
                        (\( s, t ) foldns ->
                            Dict.insert s t foldns
                        )
                        ns
                        pl
            in
            List.foldl
                (\t rbns ->
                    Result.andThen (\( rns, _ ) -> eval t rns) rbns
                )
                (Ok ( fnns, TList [] ))
                fn.body


eval : Term -> NameSpace -> Result String ( NameSpace, Term )
eval term ns =
    case term of
        TString str ->
            Ok ( ns, TString str )

        TNumber n ->
            Ok ( ns, TNumber n )

        TList terms ->
            case List.head terms of
                Nothing ->
                    Ok ( ns, TList terms )

                Just t ->
                    case eval t ns of
                        Ok ( nns, et ) ->
                            case et of
                                TFunction fn ->
                                    evalFtn fn (Util.rest terms) nns

                                TBuiltIn bif ->
                                    bif (Util.rest terms) ns

                                other ->
                                    Ok ( ns, other )

                        Err e ->
                            Err e

        TSymbol s ->
            case Dict.get s ns of
                Just t ->
                    Ok ( ns, t )

                Nothing ->
                    Err <| "symbol not found: " ++ s

        TFunction f ->
            Ok ( ns, TFunction f )

        TBuiltIn b ->
            Ok ( ns, TBuiltIn b )


sxpToTerm : Sxp -> Result (List DeadEnd) Term
sxpToTerm sxp =
    case sxp of
        STerm str ->
            run termString str

        SList sterms ->
            Result.map TList
                (List.foldr
                    (\ts rslt ->
                        case rslt of
                            Ok lst ->
                                case sxpToTerm ts of
                                    Ok term ->
                                        Ok <| term :: lst

                                    Err e ->
                                        Err e

                            Err e ->
                                Err e
                    )
                    (Ok [])
                    sterms
                )


termString : Parser Term
termString =
    oneOf
        [ parseString
        , parseNumber
        , parseSymbol
        ]


{-| parse a quoted string, without any provision for escaped quotes
-}
parseString : Parser Term
parseString =
    succeed TString
        |. symbol "\""
        |= getChompedString
            (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. end


parseSymbol : Parser Term
parseSymbol =
    succeed TSymbol
        |= getChompedString
            (chompWhile (\c -> c /= '"'))
        |. end


parseNumber : Parser Term
parseNumber =
    succeed TNumber
        |= float
        |. end



{-
   eval : Sxp -> NameSpace -> NameSpace
   eval sxp ns =
-}
