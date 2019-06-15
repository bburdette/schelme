module Prelude exposing
    ( BuiltInFn
    , SideEffectorFn
    , builtInFn
    , evalArgsBuiltIn
    , evalArgsSideEffector
    , math
    , prelude
    , mathGlossary, preludeGlossary
    )

{-| Implementation of some fundamental functions, and a few values.
Also some helpers for defining your own BuiltIn or SideEffector functions.

@docs BuiltInFn
@docs SideEffectorFn
@docs builtInFn
@docs evalArgsBuiltIn
@docs evalArgsSideEffector
@docs math
@docs prelude

-}

import Dict exposing (Dict)
import Eval exposing (evalBody, evalTerm, evalTerms)
import EvalStep
    exposing
        ( BuiltIn
        , BuiltInStep(..)
        , EvalBodyStep(..)
        , EvalTermStep(..)
        , EvalTermsStep(..)
        , NameSpace
        , GlossaryEntry
        , SideEffector
        , SideEffectorStep(..)
        , Term(..)
        , TermGlossary
        )
import Show exposing (showTerm, showTerms)
import Util exposing (rest)


{-| a NameSpace of fundamental schelme functions.
-}
prelude : Dict String (Term a)
prelude =
    Dict.empty
        |> Dict.insert "def" (TBuiltIn def)
        |> Dict.insert "defn" (TBuiltIn (builtInFn defn))
        |> Dict.insert "true" (TBool True)
        |> Dict.insert "false" (TBool False)
        |> Dict.insert "eq" (TBuiltIn (evalArgsBuiltIn eq))
        |> Dict.insert "car" (TBuiltIn (evalArgsBuiltIn car))
        |> Dict.insert "cdr" (TBuiltIn (evalArgsBuiltIn cdr))
        |> Dict.insert "cons" (TBuiltIn (evalArgsBuiltIn cons))
        |> Dict.insert "list" (TBuiltIn (evalArgsBuiltIn list))
        |> Dict.insert "quote" (TBuiltIn (builtInFn list))
        |> Dict.insert "if" (TSideEffector schelmeIf)
        |> Dict.insert "and" (TBuiltIn (evalArgsBuiltIn and))
        |> Dict.insert "or" (TBuiltIn (evalArgsBuiltIn or))
        |> Dict.insert "eval" (TSideEffector pRun)
        |> Dict.insert "do" (TSideEffector do)
        |> Dict.insert "loop" (TSideEffector loop)
        |> Dict.insert "break" (TBuiltIn (evalArgsBuiltIn break))


preludeGlossary : TermGlossary
preludeGlossary =
    Dict.empty
        |> Dict.insert "def" (GlossaryEntry "(def <symbolname> <schelme expression>)" "define a symbol.  ")
        |> Dict.insert "defn" (GlossaryEntry "(defn (<fnname> <argname1> <argname2> ...)\n<body term 1>\n...\n<body term n>)" "define a function. \n")
        |> Dict.insert "true" (GlossaryEntry "" "boolean 'true'")
        |> Dict.insert "false" (GlossaryEntry "" "boolean 'false'")
        |> Dict.insert "eq" (GlossaryEntry "(eq <arg1> <arg2> ... <argN>)" "returns boolean true or false depending on equality of the arguments.\n        ")
        |> Dict.insert "car" (GlossaryEntry "(car <list>) -> <expression> or ()" "returns the first element of a list, or if the list is empty returns the empty list.\n         ")
        |> Dict.insert "cdr" (GlossaryEntry "(cdr <list>) -> <list>" "returns the rest of the list after the first element, or the empty list if the list is empty\n         ")
        |> Dict.insert "cons" (GlossaryEntry "(cons <expression> <list>) -> <list>" """appends an element to the front of a list.  " <|"GlossaryEntry """)
        |> Dict.insert "list" (GlossaryEntry "(list <exp1> <exp2> ... <expN>) -> <list>" " \nevaluate all the passed expressions and return a list of the results.")
        |> Dict.insert "quote" (GlossaryEntry "(quote <exp1> <exp2> ... <expN>) -> <list>" "make a list of arguments without eval-ing them first.\n         ")
        |> Dict.insert "if" (GlossaryEntry "(if <boolean expression> <exp1> <exp2>)" "evaluate the boolean expression, and if its true, eval exp1.  Otherwise eval exp2.")
        |> Dict.insert "and" (GlossaryEntry "(and <exp1> <exp2> ... <expN>) -> boolean" " eval all the args and if they are all true return true - otherwise false.")
        |> Dict.insert "or" (GlossaryEntry "(or <exp1> <exp2> ... <expN>) -> boolean" " eval all the args and if one of them is true, return true")
        |> Dict.insert "do" (GlossaryEntry "(do <exp1> <exp2> ... <expN>) -> exp" " eval all the args and return the result of the last one.\ndo has its own namespace which is lost when the last expression returns.")
        |> Dict.insert "loop" (GlossaryEntry "(loop <exp1> <exp2> ... <expN>) -> <term>" "eval all the args repeatedly until a 'break' is called.\nloop has its own namespace which is lost when the last expression returns.")
        |> Dict.insert "break" (GlossaryEntry "(break <exp>) -> exp" "called from within a 'loop', causes the loop to exit returning the passed expression.")


{-| a NameSpace of mathy schelme functions.
-}
math : Dict String (Term a)
math =
    Dict.empty
        |> Dict.insert "+" (TBuiltIn (evalArgsBuiltIn plus))
        |> Dict.insert "-" (TBuiltIn (evalArgsBuiltIn minus))
        |> Dict.insert "*" (TBuiltIn (evalArgsBuiltIn multiply))
        |> Dict.insert "/" (TBuiltIn (evalArgsBuiltIn divide))
        |> Dict.insert "<" (TBuiltIn (evalArgsBuiltIn (ffbOp "<" (<))))
        |> Dict.insert "<=" (TBuiltIn (evalArgsBuiltIn (ffbOp "<=" (<=))))
        |> Dict.insert ">" (TBuiltIn (evalArgsBuiltIn (ffbOp ">" (>))))
        |> Dict.insert ">=" (TBuiltIn (evalArgsBuiltIn (ffbOp ">=" (>=))))


mathGlossary : TermGlossary
mathGlossary =
    Dict.empty
        |> Dict.insert "+" (GlossaryEntry "(+ <exp1> <exp1> ... <expN>) -> String or Number" "For strings, string concatenation.\nFor numbers, summing.\nFor strings and numbers, string concatenation.\nFor all else, error.")
        |> Dict.insert "-" (GlossaryEntry "(- <number> <number>) -> number" "subtract one number from another.")
        |> Dict.insert "*" (GlossaryEntry "(* <num1> <num2> ... <numN>) -> number" "multiply all the numbers together.")
        |> Dict.insert "/" (GlossaryEntry "(/ <number> <number>) -> number" "divide one number by another.")
        |> Dict.insert "<" (GlossaryEntry "(< <number> <number>) -> boolean" "less than.")
        |> Dict.insert "<=" (GlossaryEntry "(<= <number> <number>) -> boolean" "less than or equal to.")
        |> Dict.insert ">" (GlossaryEntry "(> <number> <number>) -> boolean" "greater than.")
        |> Dict.insert ">=" (GlossaryEntry "(< <number> <number>) -> boolean" "greater than or equal to.")


{-| function type for evalArgsBuiltIn
-}
type alias BuiltInFn a =
    NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, Term a )


{-| make a BuiltIn function where arguments are evaled before the BuiltInFn function is called.
-}
evalArgsBuiltIn : BuiltInFn a -> BuiltIn a
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
                        BuiltInArgs ns state (evalTerms ets)

            BuiltInFinal _ _ ->
                bistep

            BuiltInError _ ->
                bistep


{-| function type to pass to evalArgsSideEffector
-}
type alias SideEffectorFn a =
    NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, Term a )


{-| make a SideEffector function where arguments are evaled before the SideEffectorFn function is called.
-}
evalArgsSideEffector : SideEffectorFn a -> SideEffector a
evalArgsSideEffector fn =
    \step ->
        case step of
            SideEffectorStart ns state terms ->
                SideEffectorArgs ns state (evalTerms (EtStart ns state terms))

            SideEffectorArgs ns state ets ->
                case ets of
                    EtFinal efns enstate terms ->
                        -- we have all args, now call our 'built in'
                        case fn ns enstate terms of
                            Ok ( nebins, nestate, term ) ->
                                SideEffectorFinal nebins nestate term

                            Err e ->
                                SideEffectorError e

                    EtError e ->
                        SideEffectorError e

                    _ ->
                        SideEffectorArgs ns state (evalTerms ets)

            SideEffectorEval _ _ _ _ ->
                SideEffectorError "not expecting SideEffectorEval!"

            SideEffectorBody ns state workterms evalstep ->
                SideEffectorError "unexpected SideEffectorBody"

            SideEffectorFinal _ _ _ ->
                step

            SideEffectorError _ ->
                step


{-| make a BuiltIn function where arguments are NOT evaled before the BuiltInFn function is called. Useful for things like defn.
-}
builtInFn : BuiltInFn a -> BuiltIn a
builtInFn fn =
    \bistep ->
        case bistep of
            BuiltInStart ns state terms ->
                case fn ns state terms of
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


schelmeIf : SideEffector a
schelmeIf bistep =
    case bistep of
        SideEffectorStart ns state terms ->
            case terms of
                [ cond, br1, br2 ] ->
                    -- store the two result terms for later, and eval the cond term.
                    SideEffectorEval ns state [ br1, br2 ] (EvalStart ns state cond)

                _ ->
                    SideEffectorError ("if requires three terms <bool> <branch1> <branch2>.  received: " ++ showTerms terms)

        SideEffectorArgs ns state ets ->
            SideEffectorError "Unexpected 'SideEffectorArgs' to schelmeIf"

        SideEffectorEval ns state workterms evalstep ->
            case evalstep of
                EvalFinal efns efstate term ->
                    case workterms of
                        [ br1, br2 ] ->
                            case term of
                                TBool b ->
                                    let
                                        br =
                                            if b then
                                                br1

                                            else
                                                br2
                                    in
                                    -- no workterms indicates we're computing the return value.
                                    SideEffectorEval ns state [] (EvalStart efns state br)

                                _ ->
                                    SideEffectorError ("'if' conditional expression was not a Bool: " ++ showTerm term)

                        [] ->
                            SideEffectorFinal ns efstate term

                        _ ->
                            SideEffectorError ("invalid workterms to schelmeIf: " ++ showTerms workterms)

                EvalError e ->
                    SideEffectorError e

                _ ->
                    SideEffectorEval ns state workterms (evalTerm evalstep)

        SideEffectorBody ns state workterms evalstep ->
            SideEffectorError "if: unexpected SideEffectorBody"

        SideEffectorFinal _ _ _ ->
            bistep

        SideEffectorError _ ->
            bistep


and : BuiltInFn a
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


or : BuiltInFn a
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


eq : BuiltInFn a
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


car : BuiltInFn a
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


cdr : BuiltInFn a
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


cons : BuiltInFn a
cons ns state terms =
    case terms of
        [ ht, TList lst ] ->
            Ok ( ns, TList <| ht :: lst )

        _ ->
            Err (String.concat ("cons takes a term and a list, got: " :: List.map showTerm terms))


list : BuiltInFn a
list ns state terms =
    Ok ( ns, TList terms )


do : SideEffector a
do step =
    case step of
        SideEffectorStart ns state terms ->
            -- no args phase; straight to body.
            SideEffectorBody ns state terms (evalBody (EbStart ns state terms))

        SideEffectorArgs ns state ets ->
            SideEffectorError "do: unexpected SideEffectorArgs"

        SideEffectorEval ns state workterms evalstep ->
            SideEffectorError "do: unexpected SideEffectorEval"

        SideEffectorBody ns state workterms evalstep ->
            case evalstep of
                EbFinal efns efstate term ->
                    -- throw away namespace changes; keep state changes.
                    SideEffectorFinal ns efstate term

                EbError e ->
                    SideEffectorError e

                _ ->
                    SideEffectorBody ns state workterms (evalBody evalstep)

        SideEffectorFinal _ _ _ ->
            step

        SideEffectorError _ ->
            step


loop : SideEffector a
loop step =
    case step of
        SideEffectorStart ns state terms ->
            -- no args phase; straight to body.
            SideEffectorBody ns state terms (evalBody (EbStart ns state terms))

        SideEffectorArgs ns state ets ->
            SideEffectorError "loop: unexpected SideEffectorArgs"

        SideEffectorEval ns state workterms evalstep ->
            SideEffectorError "loop: unexpected SideEffectorEval"

        SideEffectorBody ns state workterms evalstep ->
            case evalstep of
                EbFinal efns efstate term ->
                    case term of
                        TBreak val ->
                            -- throw away namespace changes; keep state changes.
                            SideEffectorFinal ns efstate val

                        _ ->
                            -- start over at the beginning of the terms!  we are loop!
                            SideEffectorBody ns state workterms (evalBody (EbStart efns efstate workterms))

                EbError e ->
                    SideEffectorError e

                _ ->
                    SideEffectorBody ns state workterms (evalBody evalstep)

        SideEffectorFinal _ _ _ ->
            step

        SideEffectorError _ ->
            step


pRun : SideEffector a
pRun step =
    case step of
        SideEffectorStart ns state terms ->
            SideEffectorArgs ns state (evalTerms (EtStart ns state terms))

        SideEffectorArgs ns state ets ->
            case ets of
                EtFinal efns enstate terms ->
                    case terms of
                        [ term ] ->
                            SideEffectorEval ns state [] (EvalStart ns state term)

                        _ ->
                            SideEffectorError ("eval expected a single term, got: " ++ showTerms terms)

                EtError e ->
                    SideEffectorError e

                _ ->
                    SideEffectorArgs ns state (evalTerms ets)

        SideEffectorEval ns state workterms evalstep ->
            case evalstep of
                EvalFinal efns efstate term ->
                    SideEffectorFinal efns efstate term

                EvalError e ->
                    SideEffectorError e

                _ ->
                    SideEffectorEval ns state workterms (evalTerm evalstep)

        SideEffectorBody ns state workterms evalstep ->
            SideEffectorError "loop: unexpected SideEffectorBody"

        SideEffectorFinal _ _ _ ->
            step

        SideEffectorError _ ->
            step


def : BuiltIn a
def bistep =
    case bistep of
        BuiltInStart ns state terms ->
            case terms of
                [ TSymbol s, term ] ->
                    BuiltInEval ns state [ TSymbol s ] (EvalStart ns state term)

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
                    BuiltInEval ns state workterms (evalTerm evalstep)

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
defn : BuiltInFn a
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


break : BuiltInFn a
break ns state terms =
    case terms of
        [ term ] ->
            Ok ( ns, TBreak term )

        _ ->
            Err (String.concat ("break takes 1 term, got: " :: List.map showTerm terms))


plus : BuiltInFn a
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


minus : BuiltInFn a
minus ns state terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            Ok ( ns, TNumber <| x - y )

        _ ->
            Err (String.concat ("'-' require two numeric arguments.  got: " :: List.map showTerm terms))


multiply : BuiltInFn a
multiply ns state terms =
    List.foldr
        (\term rs ->
            rs
                |> Result.andThen
                    (\product ->
                        case product of
                            TNumber nproduct ->
                                case term of
                                    TNumber n ->
                                        Ok <| TNumber <| n * nproduct

                                    _ ->
                                        Err ("Invalid type for product: " ++ showTerm term)

                            _ ->
                                Err ("Invalid type for product: " ++ showTerm product)
                    )
        )
        (Ok (TNumber 1))
        terms
        |> Result.map (\tm -> ( ns, tm ))


divide : BuiltInFn a
divide ns state terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            Ok ( ns, TNumber <| x / y )

        _ ->
            Err ("'/' requires two numeric arguments.  got: " ++ showTerms terms)


ffbOp : String -> (Float -> Float -> Bool) -> BuiltInFn a
ffbOp opname f ns state terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            Ok ( ns, TBool <| f x y )

        _ ->
            Err ("'" ++ opname ++ "' requires two numeric arguments.  got: " ++ showTerms terms)
