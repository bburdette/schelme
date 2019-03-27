module PreludeStep exposing (NoEvalBuiltIn, and, car, cdr, cons, defn, eq, evalArgsBuiltIn, list, minus, or, plus, prelude, symbolNames)

import Dict exposing (Dict)
import EvalStep exposing (BuiltIn, BuiltInStep(..), EvalStep(..), EvalTermsStep(..), NameSpace, SideEffector, Term(..), eval, evalTerms, showTerm, showTerms)
import Util exposing (rest)


prelude =
    Dict.empty
        -- |> Dict.insert "def" (TBuiltIn (evalArgsBuiltIn def))
        |> Dict.insert "defn" (TBuiltIn (noEvalArgsBuiltIn defn))
        |> Dict.insert "true" (TBool True)
        |> Dict.insert "false" (TBool False)
        |> Dict.insert "eq" (TBuiltIn (evalArgsBuiltIn eq))
        |> Dict.insert "car" (TBuiltIn (evalArgsBuiltIn car))
        |> Dict.insert "cdr" (TBuiltIn (evalArgsBuiltIn cdr))
        |> Dict.insert "cons" (TBuiltIn (evalArgsBuiltIn cons))
        |> Dict.insert "list" (TBuiltIn (noEvalArgsBuiltIn list))
        --        |> Dict.insert "if" (TSideEffector schelmIf)
        |> Dict.insert "and" (TBuiltIn (evalArgsBuiltIn and))
        |> Dict.insert "or" (TBuiltIn (evalArgsBuiltIn or))
        --        |> Dict.insert "run" (TSideEffector pRun)
        |> Dict.insert "+" (TBuiltIn (evalArgsBuiltIn plus))
        |> Dict.insert "-" (TBuiltIn (evalArgsBuiltIn minus))


{-| a 'builtin' function that doesn't need to do addtional eval of terms other than its arguments
-}
type alias NoEvalBuiltIn a =
    NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, Term a )


{-| make a 'builtin' function where arguments are evaled before the NoEvalBuiltIn function is called.
-}
evalArgsBuiltIn : NoEvalBuiltIn a -> BuiltIn a
evalArgsBuiltIn nebi =
    \bistep ->
        case bistep of
            BuiltInStart ns state terms ->
                BuiltInArgs ns state (evalTerms (EtStart ns state terms))

            BuiltInEval _ _ _ _ ->
                BuiltInError "not expecting BuiltInEval!"

            BuiltInArgs ns state ets ->
                case ets of
                    EtFinal efns enstate terms ->
                        -- we have all args, now call our 'built in'
                        case nebi ns enstate terms of
                            Ok ( nebins, term ) ->
                                BuiltInFinal nebins term

                            Err e ->
                                BuiltInError e

                    EtError e ->
                        BuiltInError e

                    _ ->
                        BuiltInArgs ns state ets

            BuiltInFinal _ _ ->
                bistep

            BuiltInError _ ->
                bistep


{-| make a 'builtin' function where arguments are NOT evaled before the NoEvalBuiltIn function is called.
-}
noEvalArgsBuiltIn : NoEvalBuiltIn a -> BuiltIn a
noEvalArgsBuiltIn nebi =
    \bistep ->
        case bistep of
            BuiltInStart ns state terms ->
                case nebi ns state terms of
                    Ok ( nebins, term ) ->
                        BuiltInFinal nebins term

                    Err e ->
                        BuiltInError e

            BuiltInArgs ns state ets ->
                BuiltInError "Unexpected 'BuiltInArgs' to noEvalArgs built-in function"

            BuiltInEval _ _ _ _ ->
                BuiltInError "Unexpected 'BuiltInEval' to noEvalArgs built-in function"

            BuiltInFinal _ _ ->
                bistep

            BuiltInError _ ->
                bistep



{-
   schelmIf : SideEffector a
   schelmIf argterms ( ns, a ) =
       case argterms of
           [ boolterm, cond1, cond2 ] ->
               eval boolterm ( ns, a )
                   |> Result.andThen
                       (\( _, ebterm ) ->
                           case ebterm of
                               TBool bval ->
                                   let
                                       cond =
                                           if bval then
                                               cond1

                                           else
                                               cond2
                                   in
                                   eval cond ( ns, a ) |> Result.andThen (\( ( ns2, a2 ), resterm ) -> Ok ( ( ns2, a2 ), resterm ))

                               _ ->
                                   Err <| "first argument to 'if' must be a boolean.  got:  " ++ showTerm boolterm
                       )

           _ ->
               Err <| String.concat <| "'if' takes 3 arguments!  instead got : " :: List.map showTerm argterms
-}


and : NoEvalBuiltIn a
and ns state terms =
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
        |> Result.andThen (\br -> Ok ( ns, TBool br ))


or : NoEvalBuiltIn a
or ns state terms =
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
        |> Result.andThen (\br -> Ok ( ns, TBool br ))


eq : NoEvalBuiltIn a
eq ns state terms =
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


car : NoEvalBuiltIn a
car ns state terms =
    case List.head terms of
        Just ht ->
            case ht of
                TList items ->
                    case List.head items of
                        Just item ->
                            Ok ( ns, item )

                        Nothing ->
                            Ok ( ns, TList [] )

                _ ->
                    Err ("arg is not a list: " ++ showTerm ht)

        Nothing ->
            Err "car requires an argument"


cdr : NoEvalBuiltIn a
cdr ns state terms =
    case List.head terms of
        Just ht ->
            case ht of
                TList items ->
                    Ok ( ns, TList <| rest items )

                _ ->
                    Err ("arg is not a list: " ++ showTerm ht)

        Nothing ->
            Err "cdr requires an argument"


cons : NoEvalBuiltIn a
cons ns state terms =
    case terms of
        [ ht, TList lst ] ->
            Ok ( ns, TList <| ht :: lst )

        _ ->
            Err (String.concat ("cons takes a term and a list, got: " :: List.map showTerm terms))


list : NoEvalBuiltIn a
list ns state terms =
    Ok ( ns, TList terms )



{-
      pRun : SideEffector a
      pRun argterms ( ns, a ) =
          run argterms ( ns, a )


   def : NoEvalBuiltIn a
   def ns state terms =
       case terms of
           [ TSymbol s, term ] ->
               eval term ns
                   |> Result.andThen
                       (\( ns2, eterm ) ->
                           Ok ( Dict.insert s eterm ns, TList [] )
                       )

           _ ->
               Err (String.concat ("expected a symbol and a term as args for 'def'; got " :: List.map showTerm terms))
-}


def : BuiltIn a
def bistep =
    case bistep of
        BuiltInStart ns state terms ->
            case terms of
                [ TSymbol s, term ] ->
                    BuiltInEval ns state [ TSymbol s ] (EvalTerm ns state term)

                _ ->
                    BuiltInError "'def' requires two arguments: a Symbol and an expression."

        BuiltInArgs ns state ets ->
            BuiltInError "not expecting 'BuiltInArgs' in 'def'!"

        BuiltInEval ns state workterms evalstep ->
            case evalstep of
                EvalFinal efns enstate term ->
                    case workterms of
                        [ TSymbol s ] ->
                            BuiltInFinal (Dict.insert s term ns) (TList [])

                        _ ->
                            BuiltInError ("incorrect workterms for 'def': " ++ showTerms workterms)

                EvalError e ->
                    BuiltInError e

                _ ->
                    BuiltInEval ns state workterms (eval evalstep)

        BuiltInFinal _ _ ->
            bistep

        BuiltInError _ ->
            bistep


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
defn : NoEvalBuiltIn a
defn ns state terms =
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


plus : NoEvalBuiltIn a
plus ns state terms =
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


minus : NoEvalBuiltIn a
minus ns state terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            Ok ( ns, TNumber <| x - y )

        _ ->
            Err (String.concat ("'-' require two numeric arguments.  got: " :: List.map showTerm terms))