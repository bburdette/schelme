module EvalStep exposing (BuiltIn, BuiltInStep(..), EvalBodyStep(..), EvalFtnStep(..), EvalStep(..), EvalTermsStep(..), Function, ListStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..), compile, eval, evalBody, evalFtn, evalList, evalTerms, parseNumber, parseString, parseSymbol, run, runBody, runBodyCheck, runBodyCount, runBodyLimit, runCount, runLimit, showBuiltInStep, showEvalBodyStep, showEvalFtnStep, showEvalStep, showEvalTermsStep, showListStep, showSideEffectorStep, showTerm, showTerms, sxpToTerm, sxpsToTerms, termString)

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
        , backtrackable
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
import Util exposing (first, rest)


type alias Function a =
    { args : List String, body : List (Term a) }


type Term a
    = TString String
    | TNumber Float
    | TList (List (Term a))
    | TSymbol String
    | TBool Bool
    | TFunction (Function a)
    | TBuiltIn (BuiltIn a)
    | TSideEffector (SideEffector a)


type alias NameSpace a =
    Dict String (Term a)


type BuiltInStep a
    = BuiltInStart (NameSpace a) a (List (Term a))
    | BuiltInArgs (NameSpace a) a (EvalTermsStep a)
    | BuiltInEval (NameSpace a) a (List (Term a)) (EvalStep a)
    | BuiltInFinal (NameSpace a) (Term a)
    | BuiltInError String


type alias BuiltIn a =
    BuiltInStep a -> BuiltInStep a


showBuiltInStep : BuiltInStep a -> String
showBuiltInStep bis =
    case bis of
        BuiltInStart _ _ t ->
            "BuiltInStart - " ++ showTerms t

        BuiltInArgs _ _ t ->
            "BuiltInArgs - " ++ showEvalTermsStep t

        BuiltInEval _ _ t es ->
            "BuiltInEval - " ++ showTerms t ++ " \nevalstep: " ++ showEvalStep es

        BuiltInFinal _ t ->
            "BuiltInFinal - " ++ showTerm t

        BuiltInError s ->
            "BuiltInError - " ++ s



-- List (Term a) -> ( NameSpace a, a ) -> Result String ( NameSpace a, Term a )


type SideEffectorStep a
    = SideEffectorStart (NameSpace a) a (List (Term a))
    | SideEffectorArgs (NameSpace a) a (EvalTermsStep a)
    | SideEffectorEval (NameSpace a) a (List (Term a)) (EvalStep a)
    | SideEffectorFinal (NameSpace a) a (Term a)
    | SideEffectorError String


type alias SideEffector a =
    SideEffectorStep a -> SideEffectorStep a


showSideEffectorStep : SideEffectorStep a -> String
showSideEffectorStep ses =
    case ses of
        SideEffectorStart _ _ t ->
            "SideEffectorStart - " ++ showTerms t

        SideEffectorArgs _ _ t ->
            "SideEffectorArgs - " ++ showEvalTermsStep t

        SideEffectorEval _ _ t es ->
            "SideEffectorEval - " ++ showTerms t ++ " \nevalstep: " ++ showEvalStep es

        SideEffectorFinal _ _ t ->
            "SideEffectorFinal - " ++ showTerm t

        SideEffectorError s ->
            "SideEffectorError" ++ s


compile : String -> Result String (List (Term a))
compile text =
    Result.mapError Util.deadEndsToString
        (P.run SExpression.sSxps text
            |> Result.andThen sxpsToTerms
        )


run : NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, Term a )
run ns state terms =
    runBodyCheck (EbStart ns state terms)


runBody : EvalBodyStep a -> Result String ( NameSpace a, a, Term a )
runBody ebs =
    {- let
           _ =
               Debug.log "runBody: " ebs
       in
    -}
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, term )

        _ ->
            runBody (evalBody ebs)


runBodyCheck : EvalBodyStep a -> Result String ( NameSpace a, a, Term a )
runBodyCheck ebs =
    {- let
           _ =
               Debug.log "runBody: " <| showEvalBodyStep ebs
       in
    -}
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, term )

        _ ->
            let
                next =
                    evalBody ebs
            in
            if next == ebs then
                Err ("ebses identical! : " ++ showEvalBodyStep next)

            else
                runBodyCheck next


runCount : NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, ( Int, Term a ) )
runCount ns state terms =
    runBodyCount (EbStart ns state terms) 0


runBodyCount : EvalBodyStep a -> Int -> Result String ( NameSpace a, a, ( Int, Term a ) )
runBodyCount ebs count =
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, ( count, term ) )

        _ ->
            runBodyCount (evalBody ebs) (count + 1)


runLimit : NameSpace a -> a -> Int -> List (Term a) -> Result String ( NameSpace a, a, ( Int, Term a ) )
runLimit ns state count terms =
    runBodyLimit (EbStart ns state terms) count


runBodyLimit : EvalBodyStep a -> Int -> Result String ( NameSpace a, a, ( Int, Term a ) )
runBodyLimit ebs count =
    {- let
       _ =
           Debug.log "rbl ebs: " (showEvalBodyStep ebs)
           in
    -}
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, ( count, term ) )

        _ ->
            if count <= 0 then
                Err "step limit reached!"

            else
                runBodyLimit (evalBody ebs) (count - 1)


type EvalBodyStep a
    = EbStart (NameSpace a) a (List (Term a))
    | EbStep (NameSpace a) a (EvalStep a) (List (Term a))
    | EbFinal (NameSpace a) a (Term a)
    | EbError String


showEvalBodyStep : EvalBodyStep a -> String
showEvalBodyStep ebs =
    case ebs of
        EbStart _ _ t ->
            "EbStart - " ++ showTerms t

        EbStep _ _ es t ->
            "EbStep - " ++ showEvalStep es ++ " - remaining terms: " ++ showTerms t

        EbFinal _ _ t ->
            "EbFinal - " ++ showTerm t

        EbError s ->
            "EbError - " ++ s


evalBody : EvalBodyStep a -> EvalBodyStep a
evalBody ebs =
    case ebs of
        EbError _ ->
            ebs

        EbFinal _ _ _ ->
            ebs

        EbStart ns state terms ->
            case List.head terms of
                Nothing ->
                    EbFinal ns state (TList [])

                Just t ->
                    EbStep ns state (eval (EvalTerm ns state t)) (rest terms)

        EbStep ns state evalstep terms ->
            case evalstep of
                EvalError e ->
                    EbError e

                EvalFinal efns efstate term ->
                    case List.head terms of
                        Nothing ->
                            EbFinal efns efstate term

                        Just t ->
                            EbStep efns state (eval (EvalTerm efns efstate t)) (rest terms)

                _ ->
                    -- keep processing!
                    EbStep ns state (eval evalstep) terms


showTerm : Term a -> String
showTerm term =
    case term of
        TString str ->
            "string: " ++ str

        TNumber n ->
            "number: " ++ String.fromFloat n

        TList terms ->
            "list: " ++ "[" ++ String.concat (List.intersperse ", " (List.map showTerm terms)) ++ "]"

        TSymbol str ->
            "symbol: " ++ str

        TBool val ->
            "boolean: "
                ++ (case val of
                        True ->
                            "true"

                        False ->
                            "false"
                   )

        TFunction fn ->
            "function: " ++ String.concat (List.intersperse ", " fn.args)

        TBuiltIn bi ->
            "builtin"

        TSideEffector se ->
            "sideeffector"


showTerms : List (Term a) -> String
showTerms terms =
    "["
        ++ (String.concat <|
                List.intersperse "," <|
                    List.map showTerm terms
           )
        ++ "]"



-- steps: 1) first thing is a ftn?  2) eval args  3) eval body.


type EvalFtnStep a
    = EfStart (NameSpace a) a (Function a) (List (Term a))
    | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
    | EfBody (NameSpace a) a (EvalBodyStep a)
    | EfFinal (NameSpace a) a (Term a)
    | EfError String


showEvalFtnStep : EvalFtnStep a -> String
showEvalFtnStep efs =
    case efs of
        EfStart _ _ f t ->
            "EfStart - " ++ showTerms t

        EfArgs _ _ f ets ->
            "EfArgs - " ++ showEvalTermsStep ets

        EfBody _ _ ebs ->
            "EfBody - " ++ showEvalBodyStep ebs

        EfFinal _ _ t ->
            "EfFinal - " ++ showTerm t

        EfError s ->
            "EfError " ++ s


evalFtn : EvalFtnStep a -> EvalFtnStep a
evalFtn efs =
    case efs of
        EfStart ns state fn args ->
            EfArgs ns state fn (evalTerms (EtStart ns state args))

        EfArgs ns state fn ets ->
            case ets of
                EtFinal efns efstate terms ->
                    case Util.mbPList fn.args terms of
                        Nothing ->
                            EfError "number of args and terms don't match!"

                        Just pl ->
                            let
                                -- pair fn arg symbols and term values in the namespace.
                                argns =
                                    List.foldr
                                        (\( s, t ) foldns ->
                                            Dict.insert s t foldns
                                        )
                                        ns
                                        pl
                            in
                            -- start exing the body.
                            EfBody efns efstate (evalBody (EbStart argns efstate fn.body))

                EtError e ->
                    EfError e

                EtStart _ _ _ ->
                    EfArgs ns state fn (evalTerms ets)

                EtStep _ ->
                    EfArgs ns state fn (evalTerms ets)

        EfBody ns state eb ->
            case eb of
                EbFinal efns efstate term ->
                    EfFinal ns efstate term

                EbError e ->
                    EfError e

                EbStart _ _ _ ->
                    EfBody ns state (evalBody eb)

                EbStep _ _ _ _ ->
                    EfBody ns state (evalBody eb)

        EfFinal _ _ _ ->
            efs

        EfError _ ->
            efs


type EvalTermsStep a
    = EtStart (NameSpace a) a (List (Term a))
    | EtStep
        { ns : NameSpace a
        , state : a
        , unevaledTerms : List (Term a)
        , currentTerm : EvalStep a
        , evaledTerms : List (Term a)
        }
    | EtFinal (NameSpace a) a (List (Term a))
    | EtError String


showEvalTermsStep : EvalTermsStep a -> String
showEvalTermsStep ets =
    case ets of
        EtStart _ _ t ->
            "EtStart - " ++ showTerms t

        EtStep info ->
            "EtStep: \n  currentTerm: " ++ showEvalStep info.currentTerm ++ " \n  uevaled terms: " ++ showTerms info.unevaledTerms

        EtFinal _ _ t ->
            "EtFinal - " ++ showTerms t

        EtError s ->
            "EtError " ++ s


{-| eval terms, throwing away any changes they make to the namespace (and to 'a')
-}
evalTerms : EvalTermsStep a -> EvalTermsStep a
evalTerms ets =
    case ets of
        EtStart ns state terms ->
            case List.head terms of
                Nothing ->
                    EtFinal ns state []

                Just t ->
                    EtStep
                        { ns = ns
                        , state = state
                        , unevaledTerms = rest terms
                        , currentTerm = eval (EvalTerm ns state t)
                        , evaledTerms = []
                        }

        EtError _ ->
            ets

        EtFinal _ _ _ ->
            ets

        EtStep info ->
            case info.currentTerm of
                EvalError e ->
                    EtError e

                EvalFinal ns state term ->
                    case List.head info.unevaledTerms of
                        Nothing ->
                            EtFinal ns state (List.reverse (term :: info.evaledTerms))

                        Just t ->
                            EtStep
                                { info
                                    | state = state
                                    , unevaledTerms = rest info.unevaledTerms
                                    , currentTerm = EvalTerm ns state t
                                    , evaledTerms = term :: info.evaledTerms
                                }

                es ->
                    EtStep { info | currentTerm = eval es }


type EvalStep a
    = EvalTerm (NameSpace a) a (Term a)
    | EvalFinal (NameSpace a) a (Term a)
    | EvalListStep (ListStep a)
    | EvalError String


showEvalStep : EvalStep a -> String
showEvalStep es =
    case es of
        EvalTerm _ _ t ->
            "EvalTerm - " ++ showTerm t

        EvalFinal _ _ t ->
            "EvalFinal - " ++ showTerm t

        EvalListStep t ->
            "EvalListStep - " ++ showListStep t

        EvalError s ->
            "EvalError - " ++ s


eval : EvalStep a -> EvalStep a
eval step =
    case step of
        EvalError _ ->
            step

        EvalFinal _ _ _ ->
            step

        EvalListStep lstep ->
            let
                elstep =
                    evalList lstep
            in
            case elstep of
                ListFinal ns state term ->
                    EvalFinal ns state term

                ListError e ->
                    EvalError e

                _ ->
                    EvalListStep elstep

        EvalTerm ns state term ->
            case term of
                TList terms ->
                    EvalListStep (ListEvalStart ns state terms)

                TSymbol s ->
                    case Dict.get s ns of
                        Just t ->
                            EvalFinal ns state t

                        Nothing ->
                            EvalError <| "symbol not found: " ++ s

                TFunction f ->
                    EvalFinal ns state term

                TBuiltIn b ->
                    EvalFinal ns state term

                TSideEffector se ->
                    EvalFinal ns state term

                TString str ->
                    EvalFinal ns state term

                TNumber n ->
                    EvalFinal ns state term

                TBool b ->
                    EvalFinal ns state term


type ListStep a
    = ListEvalStart (NameSpace a) a (List (Term a))
    | ListTerm1 (NameSpace a) a (List (Term a)) (EvalStep a)
    | ListFunction (NameSpace a) a (EvalFtnStep a)
    | ListBuiltIn (NameSpace a) a (BuiltIn a) (BuiltInStep a)
    | ListSideEffector (NameSpace a) a (SideEffector a) (SideEffectorStep a)
    | ListFinal (NameSpace a) a (Term a)
    | ListError String


showListStep : ListStep a -> String
showListStep ls =
    case ls of
        ListEvalStart _ _ t ->
            "ListEvalStart - " ++ showTerms t

        ListTerm1 _ _ _ t ->
            "ListTerm1 - " ++ showEvalStep t

        ListFunction _ _ t ->
            "ListFunction - " ++ showEvalFtnStep t

        ListBuiltIn _ _ _ t ->
            "ListBuiltIn - " ++ showBuiltInStep t

        ListSideEffector _ _ _ t ->
            "ListSideEffector - " ++ showSideEffectorStep t

        ListFinal _ _ t ->
            "ListFinal - " ++ showTerm t

        ListError s ->
            "ListError" ++ s


evalList : ListStep a -> ListStep a
evalList step =
    case step of
        ListEvalStart ns state terms ->
            case List.head terms of
                Nothing ->
                    ListFinal ns state (TList [])

                Just t ->
                    ListTerm1 ns state (rest terms) (eval (EvalTerm ns state t))

        ListTerm1 ns state argterms evalStep ->
            case evalStep of
                EvalFinal ens estate term ->
                    case term of
                        TFunction fn ->
                            -- kick off function execution.
                            ListFunction ns state (evalFtn <| EfStart ns state fn argterms)

                        TBuiltIn bif ->
                            ListBuiltIn ns state bif (BuiltInStart ns state argterms)

                        TSideEffector se ->
                            ListSideEffector ns state se (se (SideEffectorStart ns state argterms))

                        other ->
                            ListError ("eval: the first element of the list should be a function!  found: " ++ showTerm other)

                EvalError e ->
                    ListError e

                _ ->
                    ListTerm1 ns state argterms (eval evalStep)

        ListFunction ns state efs ->
            case efs of
                EfFinal efns efstate term ->
                    ListFinal ns efstate term

                EfError e ->
                    ListError e

                _ ->
                    ListFunction ns state (evalFtn efs)

        ListBuiltIn ns state bif bistep ->
            case bistep of
                BuiltInFinal bins term ->
                    ListFinal bins state term

                BuiltInError e ->
                    ListError e

                _ ->
                    ListBuiltIn ns state bif (bif bistep)

        ListSideEffector ns state sef sestep ->
            case sestep of
                SideEffectorFinal sens sestate term ->
                    ListFinal sens sestate term

                SideEffectorError e ->
                    ListError e

                _ ->
                    ListSideEffector ns state sef (sef sestep)

        ListFinal _ _ _ ->
            step

        ListError _ ->
            step



{- type EvalFtnStep a
   = EfStart (NameSpace a) a (Function a) (List (Term a))
   | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
   | EfBody (NameSpace a) a (Function a) (EvalBodyStep a)
   | EfFinal (NameSpace a) a (Term a)
   | EfError String
-}


sxpToTerm : Sxp -> Result (List DeadEnd) (Term a)
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


sxpsToTerms : List Sxp -> Result (List DeadEnd) (List (Term a))
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


termString : Parser (Term a)
termString =
    oneOf
        [ parseString
        , backtrackable parseNumber
        , parseSymbol
        ]


{-| parse a quoted string, without any provision for escaped quotes
-}
parseString : Parser (Term a)
parseString =
    succeed TString
        |. symbol "\""
        |= getChompedString
            (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. end


parseSymbol : Parser (Term a)
parseSymbol =
    succeed TSymbol
        |= getChompedString
            (chompWhile (\c -> c /= '"'))
        |. end


parseNumber : Parser (Term a)
parseNumber =
    succeed TNumber
        |= float
        |. end
