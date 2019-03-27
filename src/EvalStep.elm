module EvalStep exposing (BuiltIn, BuiltInStep(..), EvalBodyStep(..), EvalFtnStep(..), EvalStep(..), EvalTermsStep(..), Function, ListStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..), compile, eval, evalBody, evalFtn, evalList, evalTerms, parseNumber, parseString, parseSymbol, showTerm, showTerms, sxpToTerm, sxpsToTerms, termString)

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
    | BuiltInEval (NameSpace a) a (List (Term a)) (EvalStep a)
    | BuiltInFinal (NameSpace a) (Term a)
    | BuiltInError String


type alias BuiltIn a =
    BuiltInStep a -> BuiltInStep a



-- List (Term a) -> ( NameSpace a, a ) -> Result String ( NameSpace a, Term a )


type SideEffectorStep a
    = SideEffectorStart (NameSpace a) a (List (Term a))
    | SideEffectorArgs (NameSpace a) a (EvalTermsStep a)
    | SideEffectorEval (NameSpace a) a (List (Term a)) (EvalStep a)
    | SideEffectorFinal (NameSpace a) a (Term a)
    | SideEffectorError String


type alias SideEffector a =
    SideEffectorStep a -> SideEffectorStep a


compile : String -> Result String (List (Term a))
compile text =
    Result.mapError Util.deadEndsToString
        (P.run SExpression.sSxps text
            |> Result.andThen sxpsToTerms
        )



{-
   run : List (Term a) -> ( NameSpace a, a ) -> Result String ( ( NameSpace a, a ), Term a )
   run terms ns =
       List.foldl
           (\term rns ->
               rns
                   |> Result.andThen
                       (\( ns2, _ ) ->
                           eval term ns2
                       )
           )
           (Ok ( ns, TList [] ))
           terms
-}


type EvalBodyStep a
    = EbStart (NameSpace a) a (List (Term a))
    | EbStep (NameSpace a) a (EvalStep a) (List (Term a))
    | EbFinal (NameSpace a) a (Term a)
    | EbError String



--evalBody : List (Term a) -> ( NameSpace a, a ) -> Result String ( ( NameSpace a, a ), Term a )


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
                    EbStep ns state (eval (EvalTerm ns state t)) (rest terms)

        EbStep ns state evalstep terms ->
            case evalstep of
                EvalError e ->
                    EbError e

                EvalFinal efns efstate term ->
                    case List.head terms of
                        Nothing ->
                            EbFinal ns efstate term

                        Just t ->
                            EbStep ns state (eval (EvalTerm efns efstate t)) (rest terms)

                _ ->
                    -- keep processing!
                    EbStep ns state (eval evalstep) (rest terms)



{-
   List.foldl
       (\term rns ->
           rns
               |> Result.andThen
                   (\( ns2, _ ) ->
                       eval term ns2
                   )
       )
       (Ok ( ns, TList [] ))
       terms
-}


showTerm : Term a -> String
showTerm term =
    case term of
        TString str ->
            "string: " ++ str

        TNumber n ->
            "number: " ++ String.fromFloat n

        TList terms ->
            "list: " ++ String.concat (List.intersperse ", " (List.map showTerm terms))

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
    String.concat <|
        List.intersperse "," <|
            List.map showTerm terms



-- steps: 1) first thing is a ftn?  2) eval args  3) eval body.


type EvalFtnStep a
    = EfStart (NameSpace a) a (Function a) (List (Term a))
    | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
    | EfBody (NameSpace a) a (EvalBodyStep a)
    | EfFinal (NameSpace a) a (Term a)
    | EfError String



-- evalFtn : ( NameSpace a, a ) -> Result String ( ( NameSpace a, a ), Term a )


evalFtn : EvalFtnStep a -> EvalFtnStep a
evalFtn efs =
    case efs of
        EfStart ns state fn args ->
            EfArgs ns state fn (evalTerms (EtStart ns state args))

        EfArgs ns state fn ets ->
            case ets of
                EtFinal efns efstate terms ->
                    -- start exing the body.
                    EfBody ns state (evalBody (EbStart efns efstate fn.body))

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



{-
   evalTerms argterms ( ans, a )
       |> Result.andThen
           (\( terms, termsval ) ->
               let
                   ns =
                       ( ans, termsval )
               in
               case Util.mbPList fn.args terms of
                   Nothing ->
                       Err "number of args and terms don't match!"

                   Just pl ->
                       let
                           fnns =
                               List.foldr
                                   (\( s, t ) ( foldns, aval ) ->
                                       ( Dict.insert s t foldns, aval )
                                   )
                                   ns
                                   pl
                       in
                       List.foldl
                           (\t rbns ->
                               Result.andThen (\( rns, _ ) -> eval t rns) rbns
                           )
                           (Ok ( fnns, TList [] ))
                           fn.body
           )
-}


type EvalTermsStep a
    = EtStart (NameSpace a) a (List (Term a))
    | EtStep
        { ns : NameSpace a
        , state : a
        , unevaledTerms : List (Term a)
        , currentTerm : EvalStep a
        , evaledTerms : List (Term a)
        }
    | EtFinal (NameSpace a) a (List (Term a))
    | EtError String


{-| eval terms, throwing away any changes they make to the namespace (and to 'a')
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
                        , currentTerm = eval (EvalTerm ns state t)
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
                            EtFinal ns state (term :: List.reverse info.evaledTerms)

                        Just t ->
                            EtStep
                                { info
                                    | state = state
                                    , unevaledTerms = rest info.unevaledTerms
                                    , currentTerm = EvalTerm ns state t
                                    , evaledTerms = term :: info.evaledTerms
                                }

                es ->
                    EtStep { info | currentTerm = eval es }



{- List.foldr
   (\term rstms ->
       rstms
           |> Result.andThen
               (\( tms, aval ) ->
                   eval term ( ns, aval )
                       |> Result.andThen
                           (\( ( etns, etval ), ettm ) -> Ok ( ettm :: tms, etval ))
               )
   )
   (Ok ( [], a ))
   terms
-}


type EvalStep a
    = EvalTerm (NameSpace a) a (Term a)
    | EvalFinal (NameSpace a) a (Term a)
    | EvalListStep (ListStep a)
    | EvalError String



-- eval : Term a -> NameSpace a -> a -> Result String ( ( NameSpace a, a ), Term a )


eval : EvalStep a -> EvalStep a
eval step =
    case step of
        EvalError _ ->
            step

        EvalFinal _ _ _ ->
            step

        EvalListStep lstep ->
            step

        EvalTerm ns state term ->
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


type ListStep a
    = ListEvalStart (NameSpace a) a (List (Term a))
    | ListTerm1 (NameSpace a) a (List (Term a)) (EvalStep a)
    | ListFunction (NameSpace a) a (EvalFtnStep a)
    | ListBuiltIn (NameSpace a) a (BuiltIn a) (BuiltInStep a)
    | ListSideEffector (NameSpace a) a (SideEffector a) (SideEffectorStep a)
    | ListFinal (NameSpace a) a (Term a)
    | ListError String


evalList : ListStep a -> ListStep a
evalList step =
    case step of
        ListEvalStart ns state terms ->
            case List.head terms of
                Nothing ->
                    ListFinal ns state (TList [])

                Just t ->
                    ListTerm1 ns state (rest terms) (eval (EvalTerm ns state t))

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
                    ListTerm1 ns state argterms (eval evalStep)

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



{- type EvalFtnStep a
   = EfStart (NameSpace a) a (Function a) (List (Term a))
   | EfArgs (NameSpace a) a (Function a) (EvalTermsStep a)
   | EfBody (NameSpace a) a (Function a) (EvalBodyStep a)
   | EfFinal (NameSpace a) a (Term a)
   | EfError String
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
