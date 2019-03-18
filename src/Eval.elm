module Eval exposing (BuiltIn, Function, NameSpace, Term(..), compile, def, eval, evalFtn, parseNumber, parseString, parseSymbol, plus, prelude, run, sxpToTerm, sxpsToTerms, termString, test1010, test123, test456, test789)

import Dict exposing (Dict)
import ParseHelp exposing (listOf)
import Parser as P
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


type alias Function =
    { args : List String, body : List Term }


type Term
    = TString String
    | TNumber Float
    | TList (List Term)
    | TSymbol String
    | TFunction Function
    | TBuiltIn BuiltIn


type alias NameSpace =
    Dict String Term


type alias BuiltIn =
    List Term -> NameSpace -> Result String ( NameSpace, Term )


def : BuiltIn
def terms ns =
    case terms of
        [ TSymbol s, term ] ->
            Ok ( Dict.insert s term ns, TList [] )

        _ ->
            Err "expected a symbol and a term as args for 'def'"


plus : BuiltIn
plus argterms ns =
    evalTerms argterms ns
        |> Result.andThen
            (\terms ->
                List.foldr
                    (\term rs ->
                        rs
                            |> Result.andThen
                                (\sum ->
                                    case sum of
                                        TString ssum ->
                                            case term of
                                                TString s ->
                                                    Ok <| TString <| String.concat [ s, ssum ]

                                                TNumber n ->
                                                    Ok <| TString <| String.concat [ String.fromFloat n, ssum ]

                                                _ ->
                                                    Err "invalid type for sum!"

                                        TNumber nsum ->
                                            case term of
                                                TString s ->
                                                    Ok <| TString <| String.concat [ s, String.fromFloat nsum ]

                                                TNumber n ->
                                                    Ok <| TNumber <| n + nsum

                                                _ ->
                                                    Err "invalid type for sum!"

                                        _ ->
                                            Err "double invalid type for sum!"
                                )
                    )
                    (Ok (TNumber 0))
                    terms
                    |> Result.map (\tm -> ( ns, tm ))
            )


prelude =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn def)
        |> Dict.insert "+" (TBuiltIn plus)


test123 =
    """5
6
"blah\""""


test456 =
    """(def a 5)
(def b "blah")
b"""


test789 =
    """(+ 57 100)"""


test1010 =
    """(def a 5)
(def b 800.80)
(+ a b)"""


compile : String -> Result String (List Term)
compile text =
    Result.mapError Util.deadEndsToString
        (P.run SExpression.sSxps text
            |> Result.andThen sxpsToTerms
        )


run : List Term -> NameSpace -> Result String ( NameSpace, Term )
run terms ns =
    List.foldl
        (\term rns ->
            rns
                |> Result.andThen
                    (\( ns2, _ ) ->
                        eval term ns2
                    )
        )
        (Ok ( ns, TList [] ))
        terms



{-
   "blah"

   2213.123512

   blah

   (defn (plus a b)
     (+ a b))

   (def blah 5)

   (plus blah blah)
-}


evalFtn : Function -> List Term -> NameSpace -> Result String ( NameSpace, Term )
evalFtn fn argterms ns =
    evalTerms argterms ns
        |> Result.andThen
            (\terms ->
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
            )


evalTerms : List Term -> NameSpace -> Result String (List Term)
evalTerms terms ns =
    List.foldr
        (\rset rstms ->
            rstms
                |> Result.andThen
                    (\tms ->
                        rset |> Result.andThen (\( etns, ettm ) -> Ok (ettm :: tms))
                    )
        )
        (Ok [])
        (List.map (\tm -> eval tm ns) terms)


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
            P.run termString str

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


sxpsToTerms : List Sxp -> Result (List DeadEnd) (List Term)
sxpsToTerms sxps =
    List.foldr
        (\sxp rs ->
            Result.andThen
                (\terms ->
                    sxpToTerm sxp
                        |> Result.andThen (\t -> Ok (t :: terms))
                )
                rs
        )
        (Ok [])
        sxps


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
