module Eval exposing (BuiltIn, Function, NameSpace, SideEffector, Term(..), compile, eval, evalFtn, evalTerms, parseNumber, parseString, parseSymbol, run, showTerm, sxpToTerm, sxpsToTerms, termString)

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


type alias StatePal a =
    { state : a
    , onEval : a -> Term a -> ( a, Bool )
    }


type alias OnEval a =
    a -> Term a -> ( a, Bool )


type alias NameSpace a =
    Dict String (Term a)


type alias Context a =
    { ns : NameSpace a
    , state : a
    }



-- ok this is covering the case where eval is paused at the 'top level'
-- but it doesn't cover pausing in a TFunction call.


type EvalRes a
    = EvalReturn (Context a) (Term a)
    | EvalPause (NameSpace a) (TermPause a)


type TermPause a
    = TermItself a (Term a)
    | TpFunction (FunctionPause a)


type FunctionPause a
    = InitialTermPause { pausedTerm : TermPause a, argTerms : List (Term a) }
    | ArgsPause { evaledTerms : List (Term a), pausedTerm : TermPause a, remainingTerms : List (Term a) }
      -- (List (Term a)) (NameSpace a) (TermPause a)
    | BodyPause { pausedTerm : TermPause a, remainingTerms : List (Term a) }


type alias BuiltIn a =
    List (Term a) -> Context a -> Result String ( NameSpace a, Term a )


type alias SideEffector a =
    List (Term a) -> Context a -> Result String ( Context a, Term a )


{-| compile a schelme program, returning a list of Terms.
-}
compile : String -> Result String (List (Term a))
compile text =
    Result.mapError Util.deadEndsToString
        (P.run SExpression.sSxps text
            |> Result.andThen sxpsToTerms
        )


{-| run a compiled schelme program (list of Terms), returning either an
execution context or a final result Term.
-}
run : OnEval a -> Context a -> List (Term a) -> Result String (EvalRes a)
run onEval ctx terms =
    List.foldl
        (\term rsEr ->
            rsEr
                |> Result.andThen
                    (\er ->
                        case er of
                            EvalPause _ _ ->
                                Ok er

                            EvalReturn erctx erterm ->
                                eval term onEval erctx
                    )
        )
        (Ok <| EvalReturn ctx <| TList [])
        terms


type ListRes a
    = ListReturn (Context a) (Term a)
    | ListPause (NameSpace a) (FunctionPause a)



{-
   = InitialTermPause { pausedTerm : TermPause a, argTerms : List (Term a) }
   | ArgsPause { evaledTerms : List (Term a), pausedTerm : TermPause a, remainingTerms : List (Term a) }
     -- (List (Term a)) (NameSpace a) (TermPause a)
   | BodyPause { pausedTerm : TermPause a, remainingTerms : List (Term a) }
-}


evalList : List (Term a) -> Context a -> OnEval a -> Result String (ListRes a)
evalList terms ctx onEval =
    case List.head terms of
        Nothing ->
            -- empty list
            Ok (ListReturn ctx (TList []))

        Just t ->
            -- eval first term.  It should be a function.
            eval t onEval ctx
                |> Result.andThen
                    (\evalRes ->
                        case evalRes of
                            -- EvalPause (NameSpace a) (TermPause a)
                            EvalPause ns tp ->
                                Ok <|
                                    ListPause ns
                                        (InitialTermPause { pausedTerm = tp, argTerms = Util.rest terms })

                            EvalReturn erctx et ->
                                case et of
                                    TFunction fn ->
                                        evalFtn fn (Util.rest terms) { ctx | state = erctx.state } onEval

                                    {-
                                       = EtReturn (List (Term a)) a
                                       | EtPause
                                           { evaledTerms : List (Term a)
                                           , pausedTerm : TermPause a
                                           , remainingTerms : List (Term a)
                                           }
                                    -}
                                    {-
                                       |> Result.andThen (\efr ->
                                         case efr of
                                           ListReturn (Context a) (Term a) -> efr
                                           ListPause lpns tp ->




                                           (\( ( fns, fna ), fterm ) ->
                                               -- throw away the final function namespace, but not the modified 'a'.
                                               Ok ( ( Tuple.first nns, fna ), fterm )
                                               )
                                    -}
                                    TBuiltIn bif ->
                                        -- built-ins only modify the namespace, not the context state.
                                        bif (Util.rest terms) ctx
                                            |> Result.map
                                                (\( bins, bitm ) ->
                                                    ListReturn { ctx | ns = bins } bitm
                                                )

                                    TSideEffector se ->
                                        se (Util.rest terms) ctx
                                            |> Result.map
                                                (\( sectx, seterm ) ->
                                                    ListReturn sectx seterm
                                                )

                                    other ->
                                        Err ("eval: the first element of the list should be a function!  found: " ++ showTerm other)
                    )


evalFtn : Function a -> List (Term a) -> Context a -> OnEval a -> Result String (ListRes a)
evalFtn fn argterms actx onEval =
    evalTerms onEval argterms actx
        |> Result.andThen
            (\etr ->
                case etr of
                    EtPause argPauseInfo ->
                        Ok <| ListPause actx.ns (ArgsPause argPauseInfo)

                    EtReturn terms state ->
                        let
                            ctx =
                                { actx | state = state }
                        in
                        case Util.mbPList fn.args terms of
                            Nothing ->
                                Err "number of args and terms don't match!"

                            Just pl ->
                                let
                                    -- put the function args into a namespace.
                                    fnctx =
                                        List.foldr
                                            (\( symbol, term ) foldctx ->
                                                -- ( foldns, aval )
                                                { foldctx | ns = Dict.insert symbol term foldctx.ns }
                                            )
                                            ctx
                                            pl
                                in
                                -- execute each term in the function body.
                                -- go until an evalpause is hit, then save the remaining body terms
                                -- after that.
                                List.foldl
                                    (\t rlrt ->
                                        rlrt
                                            |> Result.andThen
                                                (\oker ->
                                                    case oker of
                                                        ListPause ns (BodyPause bpinfo) ->
                                                            -- | EvalPause (NameSpace a) (TermPause a)
                                                            Ok <|
                                                                ListPause ns <|
                                                                    BodyPause
                                                                        { bpinfo
                                                                            | remainingTerms = t :: bpinfo.remainingTerms
                                                                        }

                                                        ListPause ns _ ->
                                                            Ok oker

                                                        ListReturn erctx erterm ->
                                                            eval t onEval erctx
                                                                |> Result.map
                                                                    (\er ->
                                                                        case er of
                                                                            EvalReturn ectx ectm ->
                                                                                ListReturn ectx ectm

                                                                            EvalPause epns epterm ->
                                                                                ListPause epns <|
                                                                                    BodyPause
                                                                                        { pausedTerm = epterm
                                                                                        , remainingTerms = []
                                                                                        }
                                                                    )
                                                )
                                    )
                                    (Ok (ListReturn fnctx <| TList []))
                                    fn.body
                                    |> Result.map
                                        (\fold ->
                                            case fold of
                                                ListPause ns (BodyPause bpinfo) ->
                                                    ListPause ns (BodyPause { bpinfo | remainingTerms = List.reverse bpinfo.remainingTerms })

                                                whatevs ->
                                                    whatevs
                                        )
            )


{-| eval a list of terms, returning a list of their result terms, and an updated state.
changes to namespace are thrown away.
-}
type EvalTermReturn a
    = EtReturn (List (Term a)) a
    | EtPause
        { evaledTerms : List (Term a)
        , pausedTerm : TermPause a
        , remainingTerms : List (Term a)
        }


evalTerms : OnEval a -> List (Term a) -> Context a -> Result String (EvalTermReturn a)
evalTerms onEval terms ctx =
    List.foldr
        (\term rstms ->
            rstms
                |> Result.andThen
                    (\etr ->
                        case etr of
                            EtPause pauseInfo ->
                                Ok (EtPause { pauseInfo | remainingTerms = term :: pauseInfo.remainingTerms })

                            EtReturn termlist termstate ->
                                eval term onEval { ctx | state = termstate }
                                    |> Result.map
                                        (\evret ->
                                            case evret of
                                                EvalPause epctx epterm ->
                                                    EtPause
                                                        { evaledTerms = termlist
                                                        , pausedTerm = epterm
                                                        , remainingTerms = []
                                                        }

                                                -- epctx epterm
                                                EvalReturn erctx erterm ->
                                                    EtReturn (erterm :: termlist) erctx.state
                                        )
                    )
        )
        (Ok (EtReturn [] ctx.state))
        terms
        |> Result.map
            (\etr ->
                case etr of
                    EtPause pauseInfo ->
                        EtPause { pauseInfo | remainingTerms = List.reverse pauseInfo.remainingTerms }

                    _ ->
                        etr
            )


eval : Term a -> OnEval a -> Context a -> Result String (EvalRes a)
eval term onEval ctx =
    let
        ( nstate, continue ) =
            onEval ctx.state term
    in
    if not continue then
        Ok <| EvalPause ctx.ns (TermItself nstate term)
        -- EvalPause { ctx | state = nstate } term

    else
        actualEval term onEval ctx


actualEval : Term a -> OnEval a -> Context a -> Result String (EvalRes a)
actualEval term onEval ctx =
    let
        ( nstate, continue ) =
            onEval ctx.state term
    in
    case term of
        TString str ->
            Ok <| EvalReturn ctx <| TString str

        TNumber n ->
            Ok <| EvalReturn ctx <| TNumber n

        TBool b ->
            Ok <| EvalReturn ctx <| TBool b

        TList terms ->
            evalList terms ctx onEval
                |> Result.map
                    (\elres ->
                        case elres of
                            ListReturn lrctx lrterm ->
                                EvalReturn { ctx | state = lrctx.state } lrterm

                            ListPause ns fp ->
                                EvalPause ns (TpFunction fp)
                    )

        TSymbol s ->
            case Dict.get s ctx.ns of
                Just t ->
                    Ok <| EvalReturn ctx t

                Nothing ->
                    Err <| "symbol not found: " ++ s

        TFunction f ->
            Ok <| EvalReturn ctx <| TFunction f

        TBuiltIn b ->
            Ok <| EvalReturn ctx <| TBuiltIn b

        TSideEffector se ->
            Ok <| EvalReturn ctx <| TSideEffector se


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


evalCounter : Int -> OnEval Int
evalCounter count =
    \c term ->
        let
            nc =
                c - 1
        in
        ( nc, not <| nc == 0 )
