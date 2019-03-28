module EvalStep exposing (BuiltIn, BuiltInStep(..), EvalBodyStep(..), EvalFtnStep(..), EvalTermStep(..), EvalTermsStep(..), Function, ListStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..), parseNumber, parseString, parseSymbol, sxpToTerm, sxpsToTerms, termString)

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
    | BuiltInEval (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | BuiltInFinal (NameSpace a) (Term a)
    | BuiltInError String


type alias BuiltIn a =
    BuiltInStep a -> BuiltInStep a


type SideEffectorStep a
    = SideEffectorStart (NameSpace a) a (List (Term a))
    | SideEffectorArgs (NameSpace a) a (EvalTermsStep a)
    | SideEffectorEval (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | SideEffectorFinal (NameSpace a) a (Term a)
    | SideEffectorError String


type alias SideEffector a =
    SideEffectorStep a -> SideEffectorStep a


type EvalBodyStep a
    = EbStart (NameSpace a) a (List (Term a))
    | EbStep (NameSpace a) a (EvalTermStep a) (List (Term a))
    | EbFinal (NameSpace a) a (Term a)
    | EbError String


type EvalFtnStep a
    = EfStart (NameSpace a) a (Function a) (List (Term a))
    | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
    | EfBody (NameSpace a) a (EvalBodyStep a)
    | EfFinal (NameSpace a) a (Term a)
    | EfError String


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


type EvalTermStep a
    = EvalStart (NameSpace a) a (Term a)
    | EvalFinal (NameSpace a) a (Term a)
    | EvalListStep (ListStep a)
    | EvalError String


type ListStep a
    = ListEvalStart (NameSpace a) a (List (Term a))
    | ListTerm1 (NameSpace a) a (List (Term a)) (EvalTermStep a)
    | ListFunction (NameSpace a) a (EvalFtnStep a)
    | ListBuiltIn (NameSpace a) a (BuiltIn a) (BuiltInStep a)
    | ListSideEffector (NameSpace a) a (SideEffector a) (SideEffectorStep a)
    | ListFinal (NameSpace a) a (Term a)
    | ListError String


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
