module Schelme.Show exposing
    ( showTerm
    , showTerms
    , showListStep
    , showEvalTermStep
    , showEvalTermsStep
    , showEvalBodyStep
    , showEvalFtnStep
    , showBuiltInStep
    , showSideEffectorStep
    )

{-| Functions for showing eval Steps as string.

@docs showTerm
@docs showTerms
@docs showListStep
@docs showEvalTermStep
@docs showEvalTermsStep
@docs showEvalBodyStep
@docs showEvalFtnStep
@docs showBuiltInStep
@docs showSideEffectorStep

-}

import Schelme.EvalStep exposing (BuiltIn, BuiltInStep(..), EvalBodyStep(..), EvalFtnStep(..), EvalTermStep(..), EvalTermsStep(..), Function, ListStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..))


{-| Show a BuiltInStep.
-}
showBuiltInStep : BuiltInStep a -> String
showBuiltInStep bis =
    case bis of
        BuiltInStart _ _ t ->
            "BuiltInStart - " ++ showTerms t

        BuiltInArgs _ _ t ->
            "BuiltInArgs - " ++ showEvalTermsStep t

        BuiltInEval _ _ t es ->
            "BuiltInEval - " ++ showTerms t ++ " \nevalstep: " ++ showEvalTermStep es

        BuiltInFinal _ t ->
            "BuiltInFinal - " ++ showTerm t

        BuiltInError s ->
            "BuiltInError - " ++ s


{-| Show a SideEffectorStep.
-}
showSideEffectorStep : SideEffectorStep a -> String
showSideEffectorStep ses =
    case ses of
        SideEffectorStart _ _ t ->
            "SideEffectorStart - " ++ showTerms t

        SideEffectorArgs _ _ t ->
            "SideEffectorArgs - " ++ showEvalTermsStep t

        SideEffectorEval _ _ t es ->
            "SideEffectorEval - " ++ showTerms t ++ " \nevaltermstep: " ++ showEvalTermStep es

        SideEffectorRequest _ _ ->
            "SideEffectorRequest"

        SideEffectorBody _ _ t es ->
            "SideEffectorBody - " ++ showTerms t ++ " \nevalbodystep: " ++ showEvalBodyStep es

        SideEffectorFinal _ _ t ->
            "SideEffectorFinal - " ++ showTerm t

        SideEffectorError s ->
            "SideEffectorError" ++ s


{-| Show a EvalBodyStep.
-}
showEvalBodyStep : EvalBodyStep a -> String
showEvalBodyStep ebs =
    case ebs of
        EbStart _ _ t ->
            "EbStart - " ++ showTerms t

        EbStep _ _ es t ->
            "EbStep - " ++ showEvalTermStep es ++ " - remaining terms: " ++ showTerms t

        EbFinal _ _ t ->
            "EbFinal - " ++ showTerm t

        EbError s ->
            "EbError - " ++ s


{-| Show a Term.
-}
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

        TBreak val ->
            "break" ++ showTerm val

        TFunction fn ->
            "function: " ++ String.concat (List.intersperse ", " fn.args)

        TBuiltIn bi ->
            "builtin"

        TSideEffector se ->
            "sideeffector"


{-| Show a List of Terms.
-}
showTerms : List (Term a) -> String
showTerms terms =
    "["
        ++ (String.concat <|
                List.intersperse "," <|
                    List.map showTerm terms
           )
        ++ "]"


{-| Show a EvalFtnStep.
-}
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


{-| Show a EvalTermsStep.
-}
showEvalTermsStep : EvalTermsStep a -> String
showEvalTermsStep ets =
    case ets of
        EtStart _ _ t ->
            "EtStart - " ++ showTerms t

        EtStep info ->
            "EtStep: \n  currentTerm: " ++ showEvalTermStep info.currentTerm ++ " \n  uevaled terms: " ++ showTerms info.unevaledTerms

        EtFinal _ _ t ->
            "EtFinal - " ++ showTerms t

        EtError s ->
            "EtError " ++ s


{-| Show a EvalTermStep.
-}
showEvalTermStep : EvalTermStep a -> String
showEvalTermStep es =
    case es of
        EvalStart _ _ t ->
            "EvalTerm - " ++ showTerm t

        EvalFinal _ _ t ->
            "EvalFinal - " ++ showTerm t

        EvalListStep t ->
            "EvalListStep - " ++ showListStep t

        EvalError s ->
            "EvalError - " ++ s


{-| Show a ListStep.
-}
showListStep : ListStep a -> String
showListStep ls =
    case ls of
        ListEvalStart _ _ t ->
            "ListEvalStart - " ++ showTerms t

        ListTerm1 _ _ _ t ->
            "ListTerm1 - " ++ showEvalTermStep t

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
