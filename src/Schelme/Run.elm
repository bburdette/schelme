module Schelme.Run exposing
    ( compile
    , run
    , evalBodyLimit
    , runBody
    , runBodyCount
    , runBodyLimit
    , runCount
    , runLimit
    , runNamedFunction
    , runFunctionStep
    )

{-| Some functions for compiling and running schelme scripts.

@docs compile
@docs run
@docs evalBodyLimit
@docs runBody
@docs runBodyCount
@docs runBodyLimit
@docs runCount
@docs runLimit
@docs runNamedFunction
@docs runFunctionStep

-}

import Dict
import Parser as P
import Schelme.Eval exposing (..)
import Schelme.EvalStep exposing (..)
import Schelme.SExpression exposing (Sxp(..), sSxps)
import Schelme.Show exposing (..)
import Schelme.Util as Util


{-| parse a string, emitting a series of Terms, which will hopefully be a valid schelme program.
-}
compile : String -> Result String (List (Term a))
compile text =
    Result.mapError Util.deadEndsToString
        (P.run sSxps text
            |> Result.andThen sxpsToTerms
        )


{-| given a namespace (for instance Prelude.prelude) and a state (see examples), and a schelme program,
run the program to completion, emitting an updated namespace, state, and final Term
-}
run : NameSpace a -> a -> List (Term a) -> Result String ( NameSpace a, a, Term a )
run ns state terms =
    runBody (EbStart ns state terms)


{-| starting with an EvalBodyStep, run to completion
-}
runBody : EvalBodyStep a -> Result String ( NameSpace a, a, Term a )
runBody ebs =
    case ebs of
        EbError e ->
            Err e

        EbFinal ns state term ->
            Ok ( ns, state, term )

        _ ->
            runBody (evalBody ebs)


{-| find a schelme function by name and run it, with the passed list of args.
-}
runNamedFunction : NameSpace a -> a -> String -> List (Term a) -> Result String ( NameSpace a, a, Term a )
runNamedFunction ns state fnname args =
    Dict.get fnname ns
        |> Result.fromMaybe ("Function not found: " ++ fnname)
        |> Result.andThen
            (\fnterm ->
                case fnterm of
                    TFunction fn ->
                        runFunctionStep (EfStart ns state fn args)

                    _ ->
                        Err <| fnname ++ " is not a schelme function!"
            )


{-| run a function to completion, returning updated namespace, state, and result term
-}
runFunctionStep : EvalFtnStep a -> Result String ( NameSpace a, a, Term a )
runFunctionStep efs =
    case efs of
        EfError e ->
            Err e

        EfFinal ns state term ->
            Ok ( ns, state, term )

        _ ->
            runFunctionStep (evalFtn efs)


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


{-| The way to go for incremental execution. Given an EvalBodyStep (the normal top
level of a running schelme program), execute it up to /count/ evals, returning the
last EvalBodyStep state.
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
