module Schelme.StateGet exposing
    ( getEvalBodyStepState
    , getEvalTermStepState
    , getEvalTermsStepState
    , getListStepState
    , getEvalFtnStepState
    , getBuiltInStepState
    , getSideEffectorStepState
    )

{-| StateGet - functions for retrieving the most nested state instance ('a') in a Step.

@docs getEvalBodyStepState
@docs getEvalTermStepState
@docs getEvalTermsStepState
@docs getListStepState
@docs getEvalFtnStepState
@docs getBuiltInStepState
@docs getSideEffectorStepState

-}

import Dict exposing (Dict)
import Schelme.EvalStep exposing (..)
import Schelme.Util exposing (first, rest)



{-

   getting the state from the current step.
   we return the state from the deepest level of execution.
   some might say it lacks a certain elegance, ha.

-}


{-| Get the state from an EvalTermStep.
-}
getEvalTermStepState : EvalTermStep a -> Maybe a
getEvalTermStepState step =
    case step of
        EvalStart _ a _ ->
            Just a

        EvalFinal _ a _ ->
            Just a

        EvalListStep lstep ->
            getListStepState lstep

        EvalError _ ->
            Nothing


{-| Get the state from an EvalTermsStep.
-}
getEvalTermsStepState : EvalTermsStep a -> Maybe a
getEvalTermsStepState step =
    case step of
        EtStart _ a _ ->
            Just a

        EtStep info ->
            case getEvalTermStepState info.currentTerm of
                Nothing ->
                    Just info.state

                Just s ->
                    Just s

        EtFinal _ a _ ->
            Just a

        EtError _ ->
            Nothing


{-| Get the state from an EvalBodyStep.
-}
getEvalBodyStepState : EvalBodyStep a -> Maybe a
getEvalBodyStepState step =
    case step of
        EbStart _ a _ ->
            Just a

        EbStep _ a ets _ ->
            case getEvalTermStepState ets of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        EbFinal _ a _ ->
            Just a

        EbError _ ->
            Nothing


{-| Get the state from an EvalFtnStep.
-}
getEvalFtnStepState : EvalFtnStep a -> Maybe a
getEvalFtnStepState step =
    case step of
        EfStart _ a _ _ ->
            Just a

        EfArgs _ a _ ets ->
            case getEvalTermsStepState ets of
                Nothing ->
                    Just a

                Just s ->
                    Just s

        EfBody _ a ebs ->
            case getEvalBodyStepState ebs of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        EfFinal _ a _ ->
            Just a

        EfError _ ->
            Nothing


{-| Get the state from an ListStep.
-}
getListStepState : ListStep a -> Maybe a
getListStepState step =
    case step of
        ListEvalStart _ a _ ->
            Just a

        ListTerm1 _ a _ ets ->
            case getEvalTermStepState ets of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        ListFunction _ a efs ->
            case getEvalFtnStepState efs of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        ListBuiltIn _ a _ bis ->
            case getBuiltInStepState bis of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        ListSideEffector _ a _ ses ->
            case getSideEffectorStepState ses of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        ListFinal _ a _ ->
            Just a

        ListError _ ->
            Nothing


{-| Get the state from an BuiltInStep.
-}
getBuiltInStepState : BuiltInStep a -> Maybe a
getBuiltInStepState step =
    case step of
        BuiltInStart _ a _ ->
            Just a

        BuiltInArgs _ a ets ->
            case getEvalTermsStepState ets of
                Nothing ->
                    Just a

                Just s ->
                    Just s

        BuiltInEval _ a _ ets ->
            case getEvalTermStepState ets of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        BuiltInFinal _ _ ->
            Nothing

        BuiltInError _ ->
            Nothing


{-| Get the state from an SideEffectorStep.
-}
getSideEffectorStepState : SideEffectorStep a -> Maybe a
getSideEffectorStepState step =
    case step of
        SideEffectorStart _ a _ ->
            Just a

        SideEffectorArgs _ a ets ->
            case getEvalTermsStepState ets of
                Nothing ->
                    Just a

                Just s ->
                    Just s

        SideEffectorEval _ a _ ets ->
            case getEvalTermStepState ets of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        SideEffectorRequest _ a ->
            Just a

        SideEffectorBody _ a _ ets ->
            case getEvalBodyStepState ets of
                Just b ->
                    Just b

                Nothing ->
                    Just a

        SideEffectorFinal _ a _ ->
            Just a

        SideEffectorError _ ->
            Nothing
