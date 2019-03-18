module Prelude exposing (def, defn, plus, prelude, symbolNames)

import Dict exposing (Dict)
import Eval exposing (BuiltIn, NameSpace, SideEffector, Term(..), eval, evalTerms, run)
import Util exposing (rest)


prelude =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn def)
        |> Dict.insert "defn" (TBuiltIn defn)
        |> Dict.insert "true" (TBool True)
        |> Dict.insert "false" (TBool True)
        |> Dict.insert "eq" (TBuiltIn eq)
        |> Dict.insert "list" (TBuiltIn list)
        |> Dict.insert "run" (TSideEffector pRun)
        |> Dict.insert "+" (TBuiltIn plus)


eq : BuiltIn a
eq argterms ( ns, a ) =
    let
        _ =
            Debug.log "eq " argterms
    in
    evalTerms argterms ( ns, a )
        |> Result.andThen
            (\( terms, na ) ->
                case List.head terms of
                    Just htm ->
                        List.foldr
                            (\term rs ->
                                if rs then
                                    htm == term

                                else
                                    False
                            )
                            True
                            (rest terms)
                            |> (\b -> Ok ( ns, TBool b ))

                    Nothing ->
                        Ok ( ns, TBool True )
            )


list : BuiltIn a
list argterms ( ns, a ) =
    Ok ( ns, TList argterms )


pRun : SideEffector a
pRun argterms ( ns, a ) =
    run argterms ( ns, a )


def : BuiltIn a
def terms ns =
    case terms of
        [ TSymbol s, term ] ->
            eval term ns
                |> Result.andThen
                    (\( ns2, eterm ) ->
                        Ok ( Dict.insert s eterm (Tuple.first ns), TList [] )
                    )

        _ ->
            Err (String.concat ("expected a symbol and a term as args for 'def'; got " :: List.map Eval.showTerm terms))


symbolNames : List (Term a) -> Result String (List String)
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
defn : BuiltIn a
defn terms ns =
    case List.head terms of
        Just (TList fnargs) ->
            symbolNames fnargs
                |> Result.andThen
                    (\names ->
                        case List.head names of
                            Just fnname ->
                                Ok ( Dict.insert fnname (TFunction { args = rest names, body = rest terms }) (Tuple.first ns), TList [] )

                            Nothing ->
                                Err "function name-arg list is empty!"
                    )

        Just _ ->
            Err "first arg to defn must be (<functionname> <arg1> <arg2> ...)"

        Nothing ->
            Err "defn requires arguments: (defn (<functionname> <arg1> <arg2> ...) <body expr 1> <body expr 2> ..."


plus : BuiltIn a
plus argterms ( ns, a ) =
    evalTerms argterms ( ns, a )
        |> Result.andThen
            (\( terms, ta ) ->
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
