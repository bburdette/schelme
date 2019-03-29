module StateSet exposing (setBuiltInStepState, setEvalBodyStepState, setEvalFtnStepState, setEvalTermStepState, setEvalTermsStepState, setListStepState, setSideEffectorStepState)

import Dict exposing (Dict)
import EvalStep exposing (..)
import Util exposing (first, rest)



{-

   setting the state in the current step.
   basically we replace every state object in the whole chain with the new state.
   this is worse than anticipated!  was state really a good idea?

-}


setEvalTermStepState : EvalTermStep a -> a -> EvalTermStep a
setEvalTermStepState step state =
    case step of
        EvalStart ns a term ->
            EvalStart ns state term

        EvalFinal ns a term ->
            EvalFinal ns state term

        EvalListStep substep ->
            EvalListStep (setListStepState substep state)

        EvalError _ ->
            step


setEvalTermsStepState : EvalTermsStep a -> a -> EvalTermsStep a
setEvalTermsStepState step state =
    case step of
        EtStart ns a termlist ->
            EtStart ns state termlist

        EtStep info ->
            EtStep
                { info
                    | state = state
                    , currentTerm = setEvalTermStepState info.currentTerm state
                }

        EtFinal ns a termlist ->
            EtFinal ns state termlist

        EtError _ ->
            step


setEvalBodyStepState : EvalBodyStep a -> a -> EvalBodyStep a
setEvalBodyStepState step state =
    case step of
        EbStart ns a termlist ->
            EbStart ns state termlist

        EbStep ns a ets termlist ->
            EbStep ns state (setEvalTermStepState ets state) termlist

        EbFinal ns a term ->
            EbFinal ns state term

        EbError _ ->
            step


setListStepState : ListStep a -> a -> ListStep a
setListStepState step state =
    case step of
        ListEvalStart ns a termlist ->
            ListEvalStart ns state termlist

        ListTerm1 ns a termlist substep ->
            ListTerm1 ns state termlist (setEvalTermStepState substep state)

        ListFunction ns a substep ->
            ListFunction ns state (setEvalFtnStepState substep state)

        ListBuiltIn ns a bi substep ->
            ListBuiltIn ns state bi (setBuiltInStepState substep state)

        ListSideEffector ns a se substep ->
            ListSideEffector ns state se (setSideEffectorStepState substep state)

        ListFinal ns a term ->
            ListFinal ns state term

        ListError _ ->
            step


setEvalFtnStepState : EvalFtnStep a -> a -> EvalFtnStep a
setEvalFtnStepState step state =
    case step of
        EfStart ns a fn termlist ->
            EfStart ns state fn termlist

        EfArgs ns a fn substep ->
            EfArgs ns state fn (setEvalTermsStepState substep state)

        EfBody ns a substep ->
            EfBody ns state (setEvalBodyStepState substep state)

        EfFinal ns a term ->
            EfFinal ns state term

        EfError _ ->
            step


setBuiltInStepState : BuiltInStep a -> a -> BuiltInStep a
setBuiltInStepState step state =
    case step of
        BuiltInStart ns a termlist ->
            BuiltInStart ns state termlist

        BuiltInArgs ns a substep ->
            BuiltInArgs ns state (setEvalTermsStepState substep state)

        BuiltInEval ns a termlist substep ->
            BuiltInEval ns state termlist (setEvalTermStepState substep state)

        BuiltInFinal ns term ->
            BuiltInFinal ns term

        BuiltInError _ ->
            step


setSideEffectorStepState : SideEffectorStep a -> a -> SideEffectorStep a
setSideEffectorStepState step state =
    case step of
        SideEffectorStart ns a termlist ->
            SideEffectorStart ns state termlist

        SideEffectorArgs ns a substep ->
            SideEffectorArgs ns state (setEvalTermsStepState substep state)

        SideEffectorEval ns a termlist substep ->
            SideEffectorEval ns state termlist (setEvalTermStepState substep state)

        SideEffectorFinal ns a term ->
            SideEffectorFinal ns state term

        SideEffectorError _ ->
            step
