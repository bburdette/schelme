module Main exposing (Model, Msg(..), main, view)

import Array as A exposing (Array)
import Browser
import Browser.Events as BE
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import EvalStep exposing (EvalBodyStep(..), NameSpace, Term(..))
import Html.Attributes as HA
import Json.Encode as JE
import Prelude as Prelude exposing (evalArgsBuiltIn, evalArgsSideEffector)
import Run exposing (compile, evalBodyLimit, runCount)
import Show exposing (showEvalBodyStep, showTerm, showTerms)
import StateGet
import StateSet
import Svg as S exposing (Svg)
import Svg.Attributes as SA


workAroundMultiLine :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (EI.Placeholder msg)
        , label : EI.Label msg
        , spellcheck : Bool
        }
    -> Element msg
workAroundMultiLine attribs mlspec =
    EI.multiline (htmlAttribute (HA.property "value" (JE.string mlspec.text)) :: attribs)
        mlspec


type Msg
    = ProgramTextChanged Int String
    | AddBot
    | Stop
    | AniFrame Float
    | Go


type alias Color =
    ( Float, Float, Float )


type alias Vec =
    ( Float, Float )


vecPlus : Vec -> Vec -> Vec
vecPlus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


type alias Bot =
    { programText : String
    , program : Result String (List (Term BotControl))
    , step : EvalBodyStep BotControl
    , position : Vec
    , velocity : Vec
    , accel : Vec
    }


type BotControl
    = BotControl
        { botidx : Int
        , bots : Array Bot
        , prints : List ( Int, String )
        }


type alias Model =
    { bots : Array Bot
    , evalsPerTurn : Int
    , go : Bool
    }


pg1 =
    "(thrust 0 0.00001)"


pg2 =
    """(def x 0)
(loop
  (if (eq x 10)
    (break 8)
    (def x (+ x 1))))"""


pg3 =
    """(loop
  (def me (myPosition)) 
  (def mex (car me)) 
  (def mey (car (cdr me)))
  (def op (getPosition 0)) 
  (def opx (car op)) 
  (def opy (car (cdr op)))
  (def direction (toPolar (- opx mex) (- opy mey)))
  (setThrust (+ 0.2 (car direction)) 0.00001))"""


pg4 =
    """(loop
  (def me (myPosition)) 
  (def mex (car me)) 
  (def mey (car (cdr me)))
  (def direction (toPolar mex mey))
  (setThrust (+ 3 (car direction)) 0.00001))"""


emptyBot : Bot
emptyBot =
    { programText = pg4
    , program = Err "uncompiled"
    , step = EbError "no program"
    , position = ( 0, 0 )
    , velocity = ( 0, 0 )
    , accel = ( 0, 0 )
    }


botColors =
    A.fromList
        [ ( 1, 0, 0 )
        , ( 0, 1, 0 )
        , ( 0, 0, 1 )
        , ( 0.75, 0, 0 )
        , ( 0, 0.75, 0 )
        , ( 0, 0, 0.75 )
        ]


colorString : ( Float, Float, Float ) -> String
colorString ( r, g, b ) =
    let
        ts =
            \v -> String.fromFloat (v * 255)
    in
    "rgb(" ++ ts r ++ "," ++ ts g ++ "," ++ ts b ++ ")"


botPositions : Float -> Int -> List ( Float, Float )
botPositions radius count =
    case count of
        0 ->
            []

        1 ->
            [ ( 0, 0 ) ]

        nz ->
            let
                ang =
                    2 * pi / toFloat nz
            in
            List.map
                (\i ->
                    let
                        a =
                            toFloat i * ang
                    in
                    ( cos a * radius, sin a * radius )
                )
                (List.range 0 (nz - 1))


getBotColor : Int -> Color
getBotColor idx =
    Maybe.withDefault ( 0, 0, 0 ) <| A.get (modBy (A.length botColors) idx) botColors


defaultBotPositions : Float -> Array Bot -> Array Bot
defaultBotPositions radius bots =
    let
        locs =
            Debug.log "locs" <|
                A.fromList
                    (botPositions radius (A.length bots))
    in
    A.indexedMap
        (\i b ->
            { b
                | position = Maybe.withDefault ( 0, 0 ) (A.get i locs)
                , accel = ( 0, 0 )
                , velocity = ( 0, 0 )
            }
        )
        bots


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


opponentCount : Prelude.NoEvalBuiltIn BotControl
opponentCount ns (BotControl bc) argterms =
    case argterms of
        [] ->
            Ok ( ns, TNumber <| toFloat (A.length bc.bots - 1) )

        _ ->
            Err (String.concat ("opponentCount takes 0 arguments!  " :: List.map showTerm argterms))


getPosition : Prelude.NoEvalBuiltIn BotControl
getPosition ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    round idx
                        |> (\i ->
                                if i < bc.botidx then
                                    i

                                else
                                    i + 1
                           )
            in
            case A.get opidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 1 argument!  " :: List.map showTerm argterms))


myPosition : Prelude.NoEvalBuiltIn BotControl
myPosition ns (BotControl bc) argterms =
    let
        _ =
            Debug.log "myposition" (showTerms argterms)
    in
    case argterms of
        [] ->
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("myPosition takes 0 arguments!  " :: List.map showTerm argterms))


getVelocity : Prelude.NoEvalBuiltIn BotControl
getVelocity ns (BotControl bc) argterms =
    case argterms of
        [] ->
            Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 0 arguments!  " :: List.map showTerm argterms))


setThrust : Prelude.NoEvalSideEffector BotControl
setThrust ns (BotControl bc) argterms =
    case argterms of
        [ TNumber angle, TNumber power ] ->
            let
                p =
                    max 0.0 (min 1.0 power)
            in
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, BotControl { bc | bots = A.set bc.botidx { bot | accel = ( cos angle * p, sin angle * p ) } bc.bots }, TList [] )

                Nothing ->
                    Err ("bot not found at index: " ++ String.fromInt bc.botidx)

        _ ->
            Err (String.concat ("thrust takes 2 arguments!  " :: List.map showTerm argterms))


print : Prelude.NoEvalSideEffector BotControl
print ns (BotControl bc) argterms =
    let
        _ =
            Debug.log ("bot " ++ String.fromInt bc.botidx ++ " printed: ") <| showTerms argterms
    in
    Ok ( ns, BotControl { bc | prints = ( bc.botidx, showTerms argterms ) :: bc.prints }, TList [] )


botftns =
    Dict.empty
        |> Dict.insert "print" (TSideEffector (evalArgsSideEffector print))
        |> Dict.insert "setThrust" (TSideEffector (evalArgsSideEffector setThrust))
        |> Dict.insert "opponentCount" (TBuiltIn (evalArgsBuiltIn opponentCount))
        |> Dict.insert "getPosition" (TBuiltIn (evalArgsBuiltIn getPosition))
        |> Dict.insert "myPosition" (TBuiltIn (evalArgsBuiltIn myPosition))
        |> Dict.insert "getVelocity" (TBuiltIn (evalArgsBuiltIn getVelocity))


botlang =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.union botftns


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bots = A.fromList []
      , evalsPerTurn = 100
      , go = False
      }
    , Cmd.none
    )


viewNamespace : NameSpace a -> Element Msg
viewNamespace ns =
    column [ width fill ] <|
        List.map
            (\( name, term ) ->
                row [ width fill, spacing 7 ] [ el [ width fill ] <| text name, el [ width fill ] <| text <| showTerm term ]
            )
            (Dict.toList ns)


viewBot : Int -> Bot -> Element Msg
viewBot idx bot =
    let
        ( r, g, b ) =
            getBotColor idx
    in
    column [ width fill ]
        [ workAroundMultiLine [ width fill, height shrink, alignTop ]
            { onChange = ProgramTextChanged idx
            , text = bot.programText
            , placeholder = Nothing
            , label = EI.labelAbove [ Font.bold ] <| text <| "Bot " ++ String.fromInt idx ++ " schelme code here: "
            , spellcheck = False
            }
        , el [ Font.bold ] <| text "Bot  color:"
        , el [ width (px 25), height (px 25), Background.color (rgb r g b) ] <| text "    "
        , case bot.program of
            Err e ->
                paragraph [ Font.color <| rgb255 204 0 0 ] [ text e ]

            Ok _ ->
                none
        , paragraph [] [ botStatus bot.step ]
        ]


botStatus : EvalBodyStep BotControl -> Element Msg
botStatus ebs =
    case ebs of
        EbFinal _ _ term ->
            text <| "stopped with final value:  " ++ showTerm term

        EbError e ->
            text <| "error:  " ++ e

        EbStart _ _ _ ->
            text "start state"

        EbStep _ _ _ _ ->
            text "running"


drawBots : Array Bot -> Element Msg
drawBots bots =
    el [ width fill, height fill ] <|
        html <|
            S.svg [ SA.width "500", SA.height "500", SA.viewBox "0 0 500 500" ] <|
                arena
                    :: List.indexedMap drawBot (A.toList bots)


arena : Svg Msg
arena =
    S.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.width "500"
        , SA.height "500"
        , SA.rx "3"
        , SA.ry "3"
        ]
        []


toSvgXY : Vec -> Vec
toSvgXY ( x, y ) =
    ( x * 250 + 250, y * 250 + 250 )


drawBot : Int -> Bot -> Svg Msg
drawBot i bot =
    let
        ( x, y ) =
            toSvgXY bot.position
    in
    S.circle [ SA.cx (String.fromFloat x), SA.cy (String.fromFloat y), SA.r "20", SA.fill (colorString (getBotColor i)) ] []


view : Model -> Element Msg
view model =
    row [ width fill ] <|
        [ column [ width fill, alignTop ] <|
            [ row [ width fill ]
                [ EI.button buttonStyle
                    { onPress = Just AddBot
                    , label = text "Add Bot"
                    }
                , EI.button buttonStyle
                    { onPress = Just Go
                    , label = text "Go"
                    }
                , EI.button buttonStyle
                    { onPress = Just Stop
                    , label = text "Stop"
                    }
                ]
            ]
                ++ List.indexedMap viewBot (A.toList model.bots)
        ]
            ++ [ drawBots model.bots ]


updateElt : Int -> (a -> a) -> Array a -> Array a
updateElt idx updfn array =
    case A.get idx array of
        Just item ->
            A.set idx (updfn item) array

        Nothing ->
            array


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgramTextChanged idx txt ->
            case A.get idx model.bots of
                Just bot ->
                    ( { model | bots = A.set idx { bot | programText = txt, program = Err "uncompiled" } model.bots }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        AddBot ->
            ( { model | bots = defaultBotPositions 0.5 <| A.push emptyBot model.bots }, Cmd.none )

        Stop ->
            ( { model | go = False }, Cmd.none )

        AniFrame millis ->
            let
                botControl =
                    BotControl { botidx = 0, bots = model.bots, prints = [] }

                -- run all bots scripts, with botcontrol as the state.
                (BotControl nbc) =
                    List.foldr
                        (\idx (BotControl bc) ->
                            case A.get idx bc.bots of
                                Just bot ->
                                    let
                                        -- replace botcontrol in the step state with an update botcontrol
                                        botstep =
                                            StateSet.setEvalBodyStepState bot.step (BotControl { bc | botidx = idx })

                                        -- run the script, updating botcontrol
                                        updstep =
                                            evalBodyLimit botstep model.evalsPerTurn
                                    in
                                    -- get the updated botcontrol.
                                    case StateGet.getEvalBodyStepState updstep of
                                        Just (BotControl updbc) ->
                                            BotControl { updbc | bots = updateElt idx (\b -> { b | step = updstep }) updbc.bots }

                                        Nothing ->
                                            -- if none, then likely an error.  update the bot step.
                                            BotControl { bc | bots = updateElt idx (\b -> { b | step = updstep }) bc.bots }

                                Nothing ->
                                    BotControl bc
                        )
                        botControl
                        (List.range 0 (A.length model.bots))

                -- physics update.
                nbots =
                    A.map
                        (\bot ->
                            let
                                -- update the state.
                                -- step = evalBodyLimit bot.step model.evalsPerTurn
                                -- mbbc = StateGet.getEvalBodyStepState bot.step
                                vel =
                                    vecPlus bot.velocity bot.accel

                                pos =
                                    vecPlus bot.position vel
                            in
                            { bot
                                | velocity = vel
                                , position = pos
                            }
                        )
                        nbc.bots
            in
            ( { model | bots = nbots }, Cmd.none )

        Go ->
            let
                compiledBots =
                    A.indexedMap
                        (\idx bot ->
                            let
                                p =
                                    compile bot.programText

                                s =
                                    p
                                        |> Result.map
                                            (\prog ->
                                                EbStart botlang (BotControl { botidx = idx, bots = model.bots, prints = [] }) prog
                                            )
                                        |> Result.withDefault (EbError "no program")

                                v =
                                    vecPlus bot.velocity
                            in
                            { bot
                                | program = p
                                , step = s
                            }
                        )
                        model.bots

                {-
                   |> Result.andThen
                       (\prog ->
                           -- Eval.runLimit preludeNColor model.color stepmax prog
                           runCount Prelue.prelude bot prog
                           )
                -}
                allCompiled =
                    List.isEmpty
                        (List.filterMap
                            (\b ->
                                Result.toMaybe b.program
                            )
                            (A.toList compiledBots)
                        )
            in
            ( { model
                | bots = defaultBotPositions 0.5 compiledBots
                , go = True
              }
            , Cmd.none
            )


main =
    Browser.element
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        , subscriptions =
            \model ->
                case model.go of
                    True ->
                        BE.onAnimationFrameDelta AniFrame

                    False ->
                        Sub.none
        }
