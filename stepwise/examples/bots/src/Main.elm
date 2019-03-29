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
import Show exposing (showTerm)
import StateGet
import StateSet
import Svg as S
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


type alias BotControl =
    { accel : ( Float, Float )
    }


type alias Vec =
    ( Float, Float )


type alias Bot =
    { programText : String
    , program : Result String (List (Term BotControl))
    , step : EvalBodyStep BotControl
    , botControl : BotControl
    , position : Vec
    , velocity : Vec
    }


type alias Model =
    { bots : Array Bot
    , evalsPerTurn : Int
    , go : Bool
    }


emptyBot : Bot
emptyBot =
    { programText = ""
    , program = Err "uncompiled"
    , botControl = { accel = ( 0, 0 ) }
    , step = EbError "no program"
    , position = ( 0, 0 )
    , velocity = ( 0, 0 )
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
            { b | position = Maybe.withDefault ( 0, 0 ) (A.get i locs) }
        )
        bots


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


opponentCount : Prelude.NoEvalSideEffector BotControl
opponentCount ns state argterms =
    case argterms of
        [] ->
            Ok ( ns, state, TList [] )

        _ ->
            Err (String.concat ("getPositions takes 0 arguments!  " :: List.map showTerm argterms))


getPosition : Prelude.NoEvalSideEffector BotControl
getPosition ns state argterms =
    case argterms of
        [] ->
            Ok ( ns, state, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 0 arguments!  " :: List.map showTerm argterms))


getVelocity : Prelude.NoEvalSideEffector BotControl
getVelocity ns state argterms =
    case argterms of
        [] ->
            Ok ( ns, state, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 0 arguments!  " :: List.map showTerm argterms))


setThrust : Prelude.NoEvalSideEffector BotControl
setThrust ns state argterms =
    case argterms of
        [ TNumber angle, TNumber power ] ->
            let
                p =
                    max 0.0 (min 1.0 power)
            in
            Ok ( ns, { state | accel = ( angle, p ) }, TList [] )

        _ ->
            Err (String.concat ("getPositions takes 0 arguments!  " :: List.map showTerm argterms))


botftns =
    Dict.empty
        |> Dict.insert "thrust" (TSideEffector (evalArgsSideEffector setThrust))


botlang =
    Dict.union Prelude.prelude botftns



{-
   setColor : Prelude.NoEvalSideEffector Color
   setColor ns a argterms =
       case argterms of
           [ TNumber r, TNumber g, TNumber b ] ->
               Ok ( ns, ( r, g, b ), TList [] )

           _ ->
               Err (String.concat ("setColor args should be 3 numbers!  " :: List.map showTerm argterms))


   preludeNColor =
       Prelude.prelude
           |> Dict.insert "setColor"
               (TSideEffector (evalArgsSideEffector setColor))
-}


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
        ]



{-
   botStatus : EvalBodyStep BotControl -> Element Msg
   botStatus ebs =
     case ebs of
        EbFinal _ _ term -> text <| "stopped with final value:  " ++ showTerm term
        EbError e -> text <| "error:  " ++ e
        EbStart ns (NameSpace a) a (List (Term a))
        EbStep (NameSpace a) a (EvalTermStep a) (List (Term a))
-}


drawBots : Array Bot -> Element Msg
drawBots bots =
    el [ width fill, height fill ] <|
        html <|
            S.svg [ SA.width "500", SA.height "500", SA.viewBox "0 0 500 500" ] <|
                List.indexedMap drawBot (A.toList bots)


toSvgXY : Vec -> Vec
toSvgXY ( x, y ) =
    ( x * 250 + 250, y * 250 + 250 )


drawBot i bot =
    let
        ( x, y ) =
            Debug.log "postion" <|
                toSvgXY bot.position

        _ =
            Debug.log "color: " (colorString (getBotColor i))
    in
    S.circle [ SA.cx (String.fromFloat x), SA.cy (String.fromFloat y), SA.r "20", SA.fill (colorString (getBotColor i)) ] []


view : Model -> Element Msg
view model =
    row [ width fill ] <|
        [ column [ width fill ] <|
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



{-

   , row [ width fill ]
       [ column [ width fill, alignTop, spacing 5 ]
           [ el [ Font.bold ] <| text "Program output:"
           , case model.programOutput of
               Err e ->
                   paragraph [ Font.color <| rgb255 204 0 0 ] [ text e ]

               Ok t ->
                   paragraph [ Font.color <| rgb255 115 210 22 ] [ text t ]
           , el [ Font.bold ] <| text "Step count: "
           , text <| String.fromInt model.count
           , el [ Font.bold ] <| text "Side effect color:"
           , el [ width (px 150), height (px 150), Background.color (rgb r g b) ] <| text "    "
           ]
       , column [ width fill, alignTop ] [ el [ Font.bold ] <| text "final namespace", viewNamespace model.finalNamespace ]
       ]

-}


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
                nb =
                    A.map
                        (\bot ->
                            let
                                _ =
                                    Debug.log "botcontrol: " (StateGet.getEvalBodyStepState bot.step)
                            in
                            { bot | step = evalBodyLimit bot.step model.evalsPerTurn }
                        )
                        model.bots
            in
            ( { model | bots = nb }, Cmd.none )

        Go ->
            let
                compiledBots =
                    A.map
                        (\bot ->
                            let
                                p =
                                    compile bot.programText

                                s =
                                    p
                                        |> Result.map
                                            (\prog ->
                                                EbStart botlang { accel = ( 0, 0 ) } prog
                                            )
                                        |> Result.withDefault (EbError "no program")
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
