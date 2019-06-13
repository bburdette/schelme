module EvalStep exposing
    ( BuiltIn
    , BuiltInStep(..)
    , EvalBodyStep(..)
    , EvalFtnStep(..)
    , EvalTermStep(..)
    , EvalTermsStep(..)
    , Function
    , ListStep(..)
    , NameSpace
    , SideEffector
    , SideEffectorStep(..)
    , Term(..)
    , sxpToTerm
    , sxpsToTerms
    )

{-| EvalStep

@docs BuiltIn
@docs BuiltInStep
@docs EvalBodyStep
@docs EvalFtnStep
@docs EvalTermStep
@docs EvalTermsStep
@docs Function
@docs ListStep
@docs NameSpace
@docs SideEffector
@docs SideEffectorStep
@docs Term
@docs sxpToTerm
@docs sxpsToTerms

-}

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


{-| A Schelme program is a list of Terms.
-}
type Term a
    = TString String
    | TNumber Float
    | TList (List (Term a))
    | TSymbol String
    | TBool Bool
    | TBreak (Term a)
    | TFunction (Function a)
    | TBuiltIn (BuiltIn a)
    | TSideEffector (SideEffector a)


{-| schelme code is always executed with a namespace, which
maps from symbol names to Terms.
-}
type alias NameSpace a =
    Dict String (Term a)


{-| EvalTermStep represents the possible states during eval of a
single Term. As with all the 'Steps', the state is modified during
eval, as is the namespace, although these changes may be thrown away by higher
levels of eval.
-}
type EvalTermStep a
    = EvalStart (NameSpace a) a (Term a)
    | EvalFinal (NameSpace a) a (Term a)
    | EvalListStep (ListStep a)
    | EvalError String


{-| EvalTermsStep is a set of states used to eval a list of Terms,
returning the list of resulting Terms. Mainly for processing function
arguments.
-}
type EvalTermsStep a
    = EtStart (NameSpace a) a (List (Term a))
    | EtStep
        { ns : NameSpace a
        , state : a
        , unevaledTerms : List (Term a)
        , currentTerm : EvalTermStep a
        , evaledTerms : List (Term a)
        }
    | EtFinal (NameSpace a) a (List (Term a))
    | EtError String


{-| EvalBodyStep is for evaling a list of Terms, throwing away the results
from all but the last Term. State is modified along the way, as is the
namespace.
-}
type EvalBodyStep a
    = EbStart (NameSpace a) a (List (Term a))
    | EbStep (NameSpace a) a (EvalTermStep a) (List (Term a))
    | EbFinal (NameSpace a) a (Term a)
    | EbError String


{-| The ListStep is the set of states for evaling a list Term. Evaling a list
usually results in function calls - fuctions defined in schelme, functions that
modify namespace but not state (builtin) and functions that modify namespace and
state (sideeffectors).
-}
type ListStep a
    = ListEvalStart (NameSpace a) a (List (Term a))
    | ListTerm1 (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | ListFunction (NameSpace a) a (EvalFtnStep a)
    | ListBuiltIn (NameSpace a) a (BuiltIn a) (BuiltInStep a)
    | ListSideEffector (NameSpace a) a (SideEffector a) (SideEffectorStep a)
    | ListFinal (NameSpace a) a (Term a)
    | ListError String


{-| a schelme function is a list of arg names and a list of Terms, which
are evaled sequentially when the function is called.
-}
type alias Function a =
    { args : List String
    , body : List (Term a)
    }


{-| The set of states during eval of a schelme function.
-}
type EvalFtnStep a
    = EfStart (NameSpace a) a (Function a) (List (Term a))
    | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
    | EfBody (NameSpace a) a (EvalBodyStep a)
    | EfFinal (NameSpace a) a (Term a)
    | EfError String


{-| A builtin function is defined in Elm. It should process any of
the BuiltInStep states, hopefully resulting in a BuiltInFinal at the
end. BuiltInFinal doesn't include the state ('a'), so any changes
mades to state are always thrown away.
-}
type alias BuiltIn a =
    BuiltInStep a -> BuiltInStep a


{-| The set of states used in defining a BuiltIn function.
-}
type BuiltInStep a
    = BuiltInStart (NameSpace a) a (List (Term a))
    | BuiltInArgs (NameSpace a) a (EvalTermsStep a)
    | BuiltInEval (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | BuiltInFinal (NameSpace a) (Term a)
    | BuiltInError String


{-| A sideeffector function processes SideEffectorSteps, and should
eventually return either a SideEffectorFinal or SideEffectorError.
Unlike the BuiltIn, the SideEffector returns a modified state ('a') in
its Final step.
-}
type alias SideEffector a =
    SideEffectorStep a -> SideEffectorStep a


{-| The steps involved in a SideEffector function. Not all these steps
have to be used. Check out the prelude for some examples.
-}
type SideEffectorStep a
    = SideEffectorStart (NameSpace a) a (List (Term a))
    | SideEffectorArgs (NameSpace a) a (EvalTermsStep a)
    | SideEffectorEval (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | SideEffectorBody (NameSpace a) a (List (Term a)) (EvalBodyStep a)
    | SideEffectorFinal (NameSpace a) a (Term a)
    | SideEffectorError String


{-| parse an s-expression, yielding a single term.
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


{-| parse a series of s-expressions, yielding a list of terms.
-}
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
