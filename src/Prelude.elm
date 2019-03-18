module Prelude exposing (def, plus, prelude, test1010, test123, test456, test789)

import Dict exposing (Dict)
import Eval exposing (BuiltIn, NameSpace, Term(..), evalTerms)


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


prelude =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn def)
        |> Dict.insert "+" (TBuiltIn plus)


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
