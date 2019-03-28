module Show exposing (showBuiltInStep, showEvalBodyStep, showEvalFtnStep, showEvalTermStep, showEvalTermsStep, showListStep, showSideEffectorStep, showTerm, showTerms)

import EvalStep exposing (BuiltIn, BuiltInStep(..), EvalBodyStep(..), EvalFtnStep(..), EvalTermStep(..), EvalTermsStep(..), Function, ListStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..))


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


showSideEffectorStep : SideEffectorStep a -> String
showSideEffectorStep ses =
    case ses of
        SideEffectorStart _ _ t ->
            "SideEffectorStart - " ++ showTerms t

        SideEffectorArgs _ _ t ->
            "SideEffectorArgs - " ++ showEvalTermsStep t

        SideEffectorEval _ _ t es ->
            "SideEffectorEval - " ++ showTerms t ++ " \nevalstep: " ++ showEvalTermStep es

        SideEffectorFinal _ _ t ->
            "SideEffectorFinal - " ++ showTerm t

        SideEffectorError s ->
            "SideEffectorError" ++ s


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
