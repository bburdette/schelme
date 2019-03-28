module Run exposing (compile, run, runBody, runBodyCheck, runBodyCount, runBodyLimit, runCount, runLimit)

import EvalStep exposing (..)
import Eval exposing (..)
import Parser as P
import SExpression exposing (Sxp(..))
import Show exposing (..)
import Util


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