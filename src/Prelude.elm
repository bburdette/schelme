module Prelude exposing (def, defn, plus, prelude, symbolNames)

import Dict exposing (Dict)
import Eval exposing (BuiltIn, EvalRes(..), NameSpace, OnEval, SideEffector, Term(..), eval, evalTerms, run, showTerm)
import Util exposing (rest)


prelude onEval =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn False (def onEval))
        |> Dict.insert "defn" (TBuiltIn False defn)
        |> Dict.insert "true" (TBool True)
        |> Dict.insert "false" (TBool False)
        |> Dict.insert "eq" (TBuiltIn True eq)
        |> Dict.insert "car" (TBuiltIn True car)
        |> Dict.insert "cdr" (TBuiltIn True cdr)
        |> Dict.insert "cons" (TBuiltIn True cons)
        |> Dict.insert "list" (TBuiltIn False list)
        |> Dict.insert "if" (TSideEffector False (schelmIf onEval))
        |> Dict.insert "and" (TBuiltIn True and)
        |> Dict.insert "or" (TBuiltIn True or)
        --        |> Dict.insert "run" (TSideEffector False (pRun onEval))
        |> Dict.insert "+" (TBuiltIn True plus)
        |> Dict.insert "-" (TBuiltIn True minus)


schelmIf : OnEval a -> SideEffector a
schelmIf onEval argterms ctx =
    case argterms of
        [ boolterm, cond1, cond2 ] ->
            eval onEval boolterm ctx
                |> Result.andThen
                    (\er ->
                        case er of
                            EvalReturn erctx erterm ->
                                case erterm of
                                    TBool bval ->
                                        let
                                            cond =
                                                if bval then
                                                    cond1

                                                else
                                                    cond2
                                        in
                                        eval onEval cond ctx |> Result.andThen (\( ( ns2, a2 ), resterm ) -> Ok ( ( ns2, a2 ), resterm ))

                                    _ ->
                                        Err <| "first argument to 'if' must be a boolean.  got:  " ++ showTerm boolterm
                    )

        _ ->
            Err <| String.concat <| "'if' takes 3 arguments!  instead got : " :: List.map showTerm argterms


and : BuiltIn a
and terms ctx =
    List.foldl
        (\term rs ->
            rs
                |> Result.andThen
                    (\b ->
                        case term of
                            TBool bv ->
                                Ok (bv && b)

                            _ ->
                                Err <| "term is not a Bool! : " ++ showTerm term
                    )
        )
        (Ok True)
        terms
        |> Result.andThen (\br -> Ok ( ctx.ns, TBool br ))


or : BuiltIn a
or terms ctx =
    List.foldl
        (\term rs ->
            rs
                |> Result.andThen
                    (\b ->
                        case term of
                            TBool bv ->
                                Ok (bv || b)

                            _ ->
                                Err <| "term is not a Bool! : " ++ showTerm term
                    )
        )
        (Ok False)
        terms
        |> Result.andThen (\br -> Ok ( ctx.ns, TBool br ))


eq : BuiltIn a
eq terms ctx =
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
                |> (\b -> Ok ( ctx.ns, TBool b ))

        Nothing ->
            Ok ( ctx.ns, TBool True )


car : BuiltIn a
car terms ctx =
    case List.head terms of
        Just ht ->
            case ht of
                TList items ->
                    case List.head items of
                        Just item ->
                            Ok ( ctx.ns, item )

                        Nothing ->
                            Ok ( ctx.ns, TList [] )

                _ ->
                    Err ("arg is not a list: " ++ showTerm ht)

        Nothing ->
            Err "car requires an argument"


cdr : BuiltIn a
cdr terms ctx =
    case List.head terms of
        Just ht ->
            case ht of
                TList items ->
                    Ok ( ctx.ns, TList <| rest items )

                _ ->
                    Err ("arg is not a list: " ++ showTerm ht)

        Nothing ->
            Err "cdr requires an argument"


cons : BuiltIn a
cons terms ctx =
    case terms of
        [ ht, TList lst ] ->
            Ok ( ctx.ns, TList <| ht :: lst )

        _ ->
            Err (String.concat ("cons takes a term and a list, got: " :: List.map showTerm terms))


list : BuiltIn a
list argterms ctx =
    Ok ( ctx.ns, TList argterms )



{-
   pRun : OnEval a -> SideEffector a
   pRun onEval argterms ctx =
       run onEval argterms ctx
-}


def : OnEval a -> BuiltIn a
def onEval terms ctx =
    case terms of
        [ TSymbol s, term ] ->
            eval term onEval ctx
                |> Result.andThen
                    (\er ->
                        case er of
                            EvalReturn rctx rterm ->
                                Ok ( Dict.insert s rterm rctx.ns, TList [] )
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
defn terms ctx =
    case List.head terms of
        Just (TList fnargs) ->
            symbolNames fnargs
                |> Result.andThen
                    (\names ->
                        case List.head names of
                            Just fnname ->
                                Ok ( Dict.insert fnname (TFunction { args = rest names, body = rest terms }) ctx.ns, TList [] )

                            Nothing ->
                                Err "function name-arg list is empty!"
                    )

        Just _ ->
            Err "first arg to defn must be (<functionname> <arg1> <arg2> ...)"

        Nothing ->
            Err "defn requires arguments: (defn (<functionname> <arg1> <arg2> ...) <body expr 1> <body expr 2> ..."


plus : BuiltIn a
plus terms ctx =
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
        |> Result.map (\tm -> ( ctx.ns, tm ))


minus : BuiltIn a
minus terms ctx =
    case terms of
        [ TNumber x, TNumber y ] ->
            Ok ( ctx.ns, TNumber <| x - y )

        _ ->
            Err (String.concat ("'-' require two numeric arguments.  got: " :: List.map showTerm terms))
