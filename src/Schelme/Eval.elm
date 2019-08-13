module Schelme.Eval exposing
    ( evalBody
    , evalFtn
    , evalList
    , evalTerm
    , evalTerms
    )

{-| functions to 'eval' steps, yielding the next step in the computation.

@docs evalBody
@docs evalFtn
@docs evalList
@docs evalTerm
@docs evalTerms

-}

import Dict
import Schelme.EvalStep exposing (..)
import Schelme.Show exposing (showTerm, showTerms)
import Schelme.Util as Util exposing (rest)


{-| Given an EvalBodyStep, compute the next EvalBodyStep.
-}
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
                    EbStep ns state (evalTerm (EvalStart ns state t)) (rest terms)

        EbStep ns state evalstep terms ->
            case evalstep of
                EvalError e ->
                    EbError e

                EvalFinal efns efstate term ->
                    case term of
                        TBreak val ->
                            -- shouldn't this be 'val' and not 'term'?
                            EbFinal efns efstate val

                        _ ->
                            case List.head terms of
                                Nothing ->
                                    EbFinal efns efstate term

                                Just t ->
                                    EbStep efns state (evalTerm (EvalStart efns efstate t)) (rest terms)

                _ ->
                    -- keep processing!
                    EbStep ns state (evalTerm evalstep) terms


{-| Given an EvalFtnStep, compute the next EvalFtnStep.
-}
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
                            EfError <|
                                "number of args and terms don't match; expected: "
                                    ++ "["
                                    ++ String.concat (List.intersperse " " fn.args)
                                    ++ "]"
                                    ++ ", got: "
                                    ++ showTerms terms

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


{-| Given an EvalTermsStep, compute the next EvalTermsStep.
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
                        , currentTerm = evalTerm (EvalStart ns state t)
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
                                    , currentTerm = EvalStart ns state t
                                    , evaledTerms = term :: info.evaledTerms
                                }

                es ->
                    EtStep { info | currentTerm = evalTerm es }


{-| Given an EvalTermStep, compute the next EvalTermStep.
-}
evalTerm : EvalTermStep a -> EvalTermStep a
evalTerm step =
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

        EvalStart ns state term ->
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

                TBreak val ->
                    EvalFinal ns state term


{-| Given an ListStep, compute the next ListStep.
-}
evalList : ListStep a -> ListStep a
evalList step =
    case step of
        ListEvalStart ns state terms ->
            case List.head terms of
                Nothing ->
                    ListFinal ns state (TList [])

                Just t ->
                    ListTerm1 ns state (rest terms) (evalTerm (EvalStart ns state t))

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
                    ListTerm1 ns state argterms (evalTerm evalStep)

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
