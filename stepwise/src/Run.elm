module Run exposing (compile, evalBodyLimit, run, runBody, runBodyCount, runBodyLimit, runCount, runLimit)

import Eval exposing (..)
import EvalStep exposing (..)
import Parser as P
import SExpression exposing (Sxp(..))
import Show exposing (..)
import Util


{-| parse a string, emitting a series of Terms, hopefully a schemle program.
-}
compile : String -> Result String (List (Term a))
compile text =
    Result.mapError Util.deadEndsToString
        (P.run SExpression.sSxps text
            |> Result.andThen sxpsToTerms
        )


{-| given a namespace (for instance Prelude.prelude) and a state (see examples), and a schelme program,
run the program to completion, emitting an update namespace, state, and final Term
-}
run : NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, Term a )
run ns state terms =
    runBody (EbStart ns state terms)



{- starting with an EvalBodyStep, run to completion -}


runBody : EvalBodyStep a -> Result String ( NameSpace a, a, Term a )
runBody ebs =
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, term )

        _ ->
            runBody (evalBody ebs)


{-| run a schelme program, emitting the usual products but also the number of evals taken
-}
runCount : NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, ( Int, Term a ) )
runCount ns state terms =
    runBodyCount (EbStart ns state terms) 0


{-| continue execution of an EvalBodyStep, returning the final Term and number of evals used.
-}
runBodyCount : EvalBodyStep a -> Int -> Result String ( NameSpace a, a, ( Int, Term a ) )
runBodyCount ebs count =
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, ( count, term ) )

        _ ->
            runBodyCount (evalBody ebs) (count + 1)


{-| run a schelme program with a max number of evals, erroring out if the max is reached.
-}
runLimit : NameSpace a -> a -> Int -> List (Term a) -> Result String ( NameSpace a, a, ( Int, Term a ) )
runLimit ns state count terms =
    runBodyLimit (EbStart ns state terms) count


{-| run a schelme EvalBodyStep with a max number of evals, erroring out if the max is reached.
-}
runBodyLimit : EvalBodyStep a -> Int -> Result String ( NameSpace a, a, ( Int, Term a ) )
runBodyLimit ebs count =
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


{-| Given an EvalBodyStep (the normal top level of a running schelme program), execute it up to
/count/ evals, returning the final EvalBodyStep state
-}
evalBodyLimit : EvalBodyStep a -> Int -> EvalBodyStep a
evalBodyLimit ebs count =
    case ebs of
        EbError e ->
            ebs

        EbFinal ns state term ->
            ebs

        _ ->
            if count <= 0 then
                ebs

            else
                evalBodyLimit (evalBody ebs) (count - 1)
