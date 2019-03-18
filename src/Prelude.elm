module Prelude exposing (def, defn, plus, prelude, symbolNames, test1010, test1011, test123, test456, test789)

import Dict exposing (Dict)
import Eval exposing (BuiltIn, NameSpace, Term(..), eval, evalTerms)
import Util exposing (rest)


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


test1011 =
    """(defn (f a b) (def c (+ a b)) (+ c a))
(f 56 57)"""


prelude =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn def)
        |> Dict.insert "defn" (TBuiltIn defn)
        |> Dict.insert "+" (TBuiltIn plus)


def : BuiltIn
def terms ns =
    case terms of
        [ TSymbol s, term ] ->
            eval term ns
                |> Result.andThen
                    (\( ns2, eterm ) ->
                        Ok ( Dict.insert s eterm ns, TList [] )
                    )

        _ ->
            Err "expected a symbol and a term as args for 'def'"


symbolNames : List Term -> Result String (List String)
symbolNames terms =
    List.foldr
        (\term rsnames ->
            rsnames
                |> Result.andThen
                    (\names ->
                        case term of
                            TSymbol name ->
                                Ok (name :: names)

                            _ ->
                                Err "term is not a symbol!"
                    )
        )
        (Ok [])
        terms


{-| defn first arg is the function name and the arg names:
(defn (<fnname> <argname1> <argname2> ...)
<body term 1>
<body term n>)
-}
defn : BuiltIn
defn terms ns =
    case List.head terms of
        Just (TList fnargs) ->
            symbolNames fnargs
                |> Result.andThen
                    (\names ->
                        case List.head names of
                            Just fnname ->
                                Ok ( Dict.insert fnname (TFunction { args = rest names, body = rest terms }) ns, TList [] )

                            Nothing ->
                                Err "function name-arg list is empty!"
                    )

        Just _ ->
            Err "first arg to defn must be (<functionname> <arg1> <arg2> ...)"

        Nothing ->
            Err "defn requires arguments: (defn (<functionname> <arg1> <arg2> ...) <body expr 1> <body expr 2> ..."


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
