module Main exposing (Bot, BotControl(..), Color, Model, Msg(..), Vec, arena, botColors, botInfo, botPixelRad, botPositions, botRadius, botStatus, botftns, botlang, buttonStyle, collide, collideArray, collideD2, colorString, defaultBotPositions, drawBot, drawBots, emptyBot, getBotColor, getOpIdx, getPosition, getVelocity, init, isDead, main, makeUrl, myPosition, opponentCount, paramParser, paramsParser, pg1, pg2, pg3, pg4, print, queryToBots, setThrust, toSvgXY, unDead, update, updateElt, updateUrl, urlBot, urlBots, vecPlus, velCollide, view, viewBot, viewNamespace, viewWinner, workAroundMultiLine)

import Array as A exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Events as BE
import Browser.Navigation as BN exposing (Key)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import EvalStep exposing (EvalBodyStep(..), NameSpace, GlossaryEntry, Term(..), TermGlossary)
import Html.Attributes as HA
import Json.Encode as JE
import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=))
import Prelude as Prelude exposing (BuiltInFn, evalArgsBuiltIn, evalArgsSideEffector)
import Random
import Random.List as RL
import Run exposing (compile, evalBodyLimit, runCount)
import Show exposing (showEvalBodyStep, showTerm, showTerms)
import StateGet
import StateSet
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Url exposing (Url)


type Msg
    = ProgramTextChanged Int String
    | AddBot
    | DeleteBot Int
    | Stop
    | AniFrame Float
    | Sumo Bool
    | ShowCode Bool
    | Go
    | RandomBPs (List ( Float, Float ))
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | RightPanelViewSelected RightPanelView
    | ShowPreludeFtns Bool
    | ShowBotFtns Bool


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
    , dead : Bool
    }


botRadius =
    0.1


botSpawnRadius =
    0.5


botPixelRad =
    String.fromInt <| round <| 250 * botRadius


type BotControl
    = BotControl
        { botidx : Int
        , bots : Array Bot
        , prints : Dict Int (List String)
        }


type RightPanelView
    = Game
    | CommandGlossary


type alias Model =
    { bots : Array Bot
    , prints : Dict Int (List String)
    , evalsPerTurn : Int
    , navkey : BN.Key
    , go : Bool
    , sumo : Bool
    , showCode : Bool
    , rightPanelView : RightPanelView
    , showPreludeFtns : Bool
    , showBotFtns : Bool
    }


makeUrl : Model -> String
makeUrl model =
    "?"
        ++ "sumo="
        ++ (if model.sumo then
                "1"

            else
                "0"
           )
        ++ "&botcount="
        ++ String.fromInt (A.length model.bots)
        ++ String.concat
            (List.indexedMap
                botInfo
                (A.toList model.bots)
            )


botInfo : Int -> Bot -> String
botInfo idx bot =
    "&botProg" ++ String.fromInt idx ++ "=" ++ Url.percentEncode bot.programText


urlBot : Dict String String -> Int -> Maybe Bot
urlBot dict idx =
    Dict.get ("botProg" ++ String.fromInt idx) dict
        |> Maybe.map
            (\prog ->
                { emptyBot | programText = Url.percentDecode prog |> Maybe.withDefault prog }
            )


urlBots : Dict String String -> Array Bot
urlBots pd =
    case Dict.get "botcount" pd |> Maybe.andThen String.toInt of
        Just count ->
            List.filterMap (urlBot pd) (List.range 0 count)
                |> A.fromList
                |> defaultBotPositions botSpawnRadius

        Nothing ->
            A.fromList []


queryToBots : String -> Array Bot
queryToBots params =
    P.run paramsParser params
        |> Result.toMaybe
        |> Maybe.map urlBots
        |> Maybe.withDefault A.empty


paramParser : P.Parser ( String, String )
paramParser =
    P.succeed (\a b -> ( a, b ))
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '='))
        |. P.symbol "="
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '&'))


paramsParser : P.Parser (Dict String String)
paramsParser =
    P.succeed (\a b -> Dict.fromList <| a :: b)
        |= paramParser
        |= listOf
            (P.succeed identity
                |. P.symbol "&"
                |= paramParser
            )



-- various test programs


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
    """(def count 150)
(loop
  (if (eq count 0) (break "") "")
  (def count (- count 1))
  (def me (myPosition)) 
  (def mex (car me)) 
  (def mey (car (cdr me)))
  (def direction (toPolar mex mey))
  (setThrust (+ (/ 3.1415 2) (car direction)) 0.00001))
(loop
  (def me (myPosition)) 
  (def mex (car me)) 
  (def mey (car (cdr me)))
  (def direction (toPolar mex mey))
  (setThrust (+ 3.1415 (car direction)) 0.00001))"""


emptyBot : Bot
emptyBot =
    { programText = pg4
    , program = Err "uncompiled"
    , step = EbError "no program"
    , position = ( 0, 0 )
    , velocity = ( 0, 0 )
    , accel = ( 0, 0 )
    , dead = False
    }


botColors =
    A.fromList
        [ ( 1, 0, 0 )
        , ( 0, 1, 0 )
        , ( 0, 0, 1 )
        , ( 0.75, 0.25, 0 )
        , ( 0, 0.75, 0.25 )
        , ( 0.25, 0, 0.75 )
        , ( 0.75, 0.25, 0.5 )
        , ( 0.5, 0.75, 0.25 )
        , ( 0.25, 0.5, 0.75 )
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


applyBotPositions : Array ( Float, Float ) -> Array Bot -> Array Bot
applyBotPositions locs bots =
    A.indexedMap
        (\i b ->
            { b
                | position = Maybe.withDefault b.position (A.get i locs)
                , accel = ( 0, 0 )
                , velocity = ( 0, 0 )
            }
        )
        bots


defaultBotPositions : Float -> Array Bot -> Array Bot
defaultBotPositions radius bots =
    let
        locs =
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


unDead : Array Bot -> Array Bot
unDead bots =
    A.map (\bot -> { bot | dead = False }) bots


buttonStyle =
    [ BG.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]



{-
   liveBots : Array Bot -> Array Int
   liveBots bots =
       A.toIndexedList bots
           |> List.filterMap
               (\( i, b ) ->
                   if b.dead then
                       Nothing

                   else
                       Just i
               )
           |> A.fromList
-}


{-| includes dead bots!
-}
opponentCount : Prelude.BuiltInFn BotControl
opponentCount ns (BotControl bc) argterms =
    case argterms of
        [] ->
            Ok ( ns, TNumber <| toFloat (A.length bc.bots - 1) )

        _ ->
            Err (String.concat ("opponentCount takes 0 arguments!  " :: List.map showTerm argterms))


getOpIdx : Int -> Int -> Int -> Maybe Int
getOpIdx robot rqidx count =
    let
        i =
            modBy count (1 + rqidx + robot)
    in
    if i == robot then
        Nothing

    else
        Just i


{-| if a bot is dead, returns (list)
-}
getPosition : Prelude.BuiltInFn BotControl
getPosition ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    getOpIdx bc.botidx (round idx) (A.length bc.bots)
            in
            case opidx |> Maybe.andThen (\oi -> A.get oi bc.bots) of
                Just bot ->
                    if bot.dead then
                        Ok ( ns, TList [] )

                    else
                        Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 1 argument!  " :: List.map showTerm argterms))


myPosition : Prelude.BuiltInFn BotControl
myPosition ns (BotControl bc) argterms =
    case argterms of
        [] ->
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("myPosition takes 0 arguments!  " :: List.map showTerm argterms))


myVelocity : Prelude.BuiltInFn BotControl
myVelocity ns (BotControl bc) argterms =
    case argterms of
        [] ->
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.velocity, TNumber <| Tuple.second bot.velocity ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("myVelocity takes 0 arguments!  Got:" :: List.map showTerm argterms))


getVelocity : Prelude.BuiltInFn BotControl
getVelocity ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    getOpIdx bc.botidx (round idx) (A.length bc.bots)
            in
            case opidx |> Maybe.andThen (\oi -> A.get oi bc.bots) of
                Just bot ->
                    if bot.dead then
                        Ok ( ns, TList [] )

                    else
                        Ok ( ns, TList [ TNumber <| Tuple.first bot.velocity, TNumber <| Tuple.second bot.velocity ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getVelocity takes 1 argument!  Got:" :: List.map showTerm argterms))


setThrust : Prelude.SideEffectorFn BotControl
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


print : Prelude.SideEffectorFn BotControl
print ns (BotControl bc) argterms =
    {- let
       _ = Debug.log ("bot " ++ String.fromInt bc.botidx ++ " printed: ") <| showTerms argterms
           in
    -}
    Ok
        ( ns
        , BotControl
            { bc
                | prints =
                    Dict.get bc.botidx bc.prints
                        |> Maybe.withDefault []
                        |> (::) (showTerms argterms)
                        |> List.take 20
                        |> (\strs -> Dict.insert bc.botidx strs bc.prints)
            }
        , TList []
        )


botftns =
    Dict.empty
        |> Dict.insert "print" (TSideEffector (evalArgsSideEffector print))
        |> Dict.insert "setThrust" (TSideEffector (evalArgsSideEffector setThrust))
        |> Dict.insert "opponentCount" (TBuiltIn (evalArgsBuiltIn opponentCount))
        |> Dict.insert "getPosition" (TBuiltIn (evalArgsBuiltIn getPosition))
        |> Dict.insert "myPosition" (TBuiltIn (evalArgsBuiltIn myPosition))
        |> Dict.insert "getVelocity" (TBuiltIn (evalArgsBuiltIn getVelocity))
        |> Dict.insert "myVelocity" (TBuiltIn (evalArgsBuiltIn myVelocity))
        |> Dict.insert "toPolar" (TBuiltIn (evalArgsBuiltIn toPolar))
        |> Dict.insert "fromPolar" (TBuiltIn (evalArgsBuiltIn fromPolar))


botreference : TermGlossary
botreference =
    Dict.empty
        |> Dict.insert "print"
            (GlossaryEntry
                "(print <expression>) -> ()"
                "prints a debug message"
            )
        |> Dict.insert "setThrust"
            (GlossaryEntry
                "(setThrust <radians> <acceleration>"
                "set direction and amount of acceleration"
            )
        |> Dict.insert "opponentCount"
            (GlossaryEntry
                "(opponentCount) -> <number>"
                "returns the number of live opponents"
            )
        |> Dict.insert "getPosition"
            (GlossaryEntry
                "(getPosition <num index>) -> (<num x>, <num y>)"
                "returns the XY position of an opponent"
            )
        |> Dict.insert "myPosition"
            (GlossaryEntry
                "(myPosition) -> (<num x>, <num y>)"
                "returns the XY position of the 'self' bot"
            )
        |> Dict.insert "getVelocity"
            (GlossaryEntry
                "(getVelocity <num index>) -> (<num x>, <num y>)"
                "given an index, returns the XY vector of the opponent's velocity."
            )
        |> Dict.insert "myVelocity"
            (GlossaryEntry
                "(myVelocity) -> (<num x>, <num y>)"
                "returns the XY velocity vector of 'self'"
            )
        |> Dict.insert "toPolar"
            (GlossaryEntry
                "(toPolar <num x>, <num y>) -> (<radians>, <distance>)"
                "convert XY to Angle,Radius"
            )
        |> Dict.insert "fromPolar"
            (GlossaryEntry
                "(fromPolar <radians>, <distance>) -> (<num x>, <num y>)"
                "convert Angle,Radius to XY"
            )


allreference =
    botreference
        |> Dict.union Prelude.preludeGlossary
        |> Dict.union Prelude.mathGlossary


botlang =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.union botftns


fromPolar : BuiltInFn a
fromPolar ns state terms =
    case terms of
        [ TNumber a, TNumber m ] ->
            Ok ( ns, TList [ TNumber <| cos a * m, TNumber <| sin a * m ] )

        _ ->
            Err ("fromPolar expected two numbers, got: " ++ showTerms terms)



{-
               |
     y / -x    |      y / x
               |
               |
   -----------------------------
               |
               |
    -y / -x    |     -y / x
               |
               |
-}


toPolar : BuiltInFn a
toPolar ns state terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            let
                a =
                    atan (y / x)
                        + (if x < 0 then
                            pi

                           else
                            0
                          )

                m =
                    sqrt (x * x + y * y)
            in
            Ok ( ns, TList [ TNumber a, TNumber m ] )

        _ ->
            Err ("toPolar expected two numbers, got: " ++ showTerms terms)


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { bots =
            url.query
                |> Maybe.map queryToBots
                |> Maybe.withDefault A.empty
      , evalsPerTurn = 100
      , go = False
      , navkey = key
      , prints = Dict.empty
      , sumo = True
      , showCode = True
      , rightPanelView = Game
      , showPreludeFtns = True
      , showBotFtns = True
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


viewBot : Bool -> Dict Int (List String) -> Int -> Bot -> Element Msg
viewBot showCode prints idx bot =
    let
        ( r, g, b ) =
            getBotColor idx
    in
    column [ width fill, Border.widthXY 0 2 ] <|
        [ row [ width fill, spacing 7 ]
            [ el [ Font.bold ] <| text <| "Bot " ++ String.fromInt idx
            , el [ width (px 25), height (px 25), BG.color (rgb r g b) ] <| text "    "
            , EI.button (alignRight :: buttonStyle)
                { onPress = Just <| DeleteBot idx
                , label = text "Delete"
                }
            ]
        , if showCode then
            workAroundMultiLine [ width fill, height (maximum 500 shrink), alignTop ]
                { onChange = ProgramTextChanged idx
                , text = bot.programText
                , placeholder = Nothing
                , label = EI.labelAbove [ Font.bold ] <| text "schelme code: "
                , spellcheck = False
                }

          else
            none
        , case bot.program of
            Err e ->
                paragraph [ Font.color <| rgb255 204 0 0 ] [ text e ]

            Ok _ ->
                none
        , paragraph [] [ botStatus bot.step ]
        ]
            ++ [ column [ scrollbarY, height <| maximum 130 shrink, width fill ] <|
                    List.map text (Maybe.withDefault [] (Dict.get idx prints))
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


drawBots : Model -> Element Msg
drawBots model =
    el [ width fill, height fill ] <|
        html <|
            S.svg [ SA.width "500", SA.height "500", SA.viewBox "0 0 500 500" ] <|
                arena model.sumo
                    :: List.indexedMap drawBot (A.toList model.bots)


arena : Bool -> Svg Msg
arena sumo =
    S.rect
        ([ SA.x "0"
         , SA.y "0"
         , SA.width "500"
         , SA.height "500"
         , SA.rx "3"
         , SA.ry "3"
         ]
            ++ (if sumo then
                    [ SA.stroke "darkred"
                    , SA.strokeWidth "10"
                    ]

                else
                    []
               )
        )
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
    S.circle
        [ SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        , SA.r botPixelRad
        , SA.fill (colorString (getBotColor i))
        ]
        []


rda =
    [ Border.width 10, Border.color <| rgb 1 1 1, width <| fillPortion 3 ]


viewGlossary : Model -> Element Msg
viewGlossary model =
    let
        ref =
            case ( model.showBotFtns, model.showPreludeFtns ) of
                ( True, True ) ->
                    allreference

                ( True, False ) ->
                    botreference

                ( False, True ) ->
                    Dict.union Prelude.mathGlossary Prelude.preludeGlossary

                ( False, False ) ->
                    Dict.empty
    in
    column [ width fill, spacing 7 ] <|
        row [ width fill ]
            [ EI.checkbox []
                { onChange = ShowBotFtns
                , icon = EI.defaultCheckbox
                , checked = model.showBotFtns
                , label = EI.labelLeft [] <| text "bot functions"
                }
            , EI.checkbox []
                { onChange = ShowPreludeFtns
                , icon = EI.defaultCheckbox
                , checked = model.showPreludeFtns
                , label = EI.labelLeft [] <| text "prelude functions"
                }
            ]
            :: List.map
                (\( name, termref ) ->
                    row [ width fill, spacing 7 ]
                        [ el [ width fill, Font.bold, alignTop ] <| text name
                        , column [ width <| fillPortion 5 ]
                            [ el [ Font.italic ] <| text termref.syntax
                            , paragraph [] [ text termref.description ]
                            ]
                        ]
                )
                (Dict.toList ref)


view : Model -> Element Msg
view model =
    row [ width fill, height fill ] <|
        [ column [ width fill, alignTop, height fill, scrollbarY ] <|
            row [ width fill, spacing 5 ]
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
                , newTabLink [ Font.color (rgb 0 0 1), Font.underline, alignRight ]
                    { url = "https://github.com/bburdette/schelme"
                    , label = text "github"
                    }
                ]
                :: row [ spacing 5 ]
                    [ EI.checkbox [ spacing 5 ]
                        { onChange = Sumo
                        , icon = EI.defaultCheckbox
                        , checked = model.sumo
                        , label = EI.labelLeft [] <| text "sumo mode"
                        }
                    , EI.checkbox [ spacing 5 ]
                        { onChange = ShowCode
                        , icon = EI.defaultCheckbox
                        , checked = model.showCode
                        , label = EI.labelLeft [] <| text "show code"
                        }
                    ]
                :: List.indexedMap (viewBot model.showCode model.prints) (A.toList model.bots)
        , column [ width fill, alignTop ]
            [ EI.radioRow
                (spacing 5 :: rda)
                { onChange = RightPanelViewSelected
                , options =
                    [ EI.option Game (text "Game")
                    , EI.option CommandGlossary (text "Command Glossary")
                    ]
                , selected = Just model.rightPanelView
                , label = EI.labelLeft [ centerY ] <| text "show:"
                }
            , case model.rightPanelView of
                Game ->
                    column [ width fill, alignTop ]
                        [ drawBots model
                        , if model.sumo then
                            viewWinner model.bots

                          else
                            none
                        ]

                CommandGlossary ->
                    viewGlossary model
            ]
        ]


viewWinner : Array Bot -> Element Msg
viewWinner bots =
    let
        winners =
            List.filterMap identity <|
                List.indexedMap
                    (\i bot ->
                        if not bot.dead then
                            Just i

                        else
                            Nothing
                    )
                    (A.toList bots)
    in
    case winners of
        [ winner ] ->
            el [ Font.color <| rgb 1 0 0, Font.bold ] <| text <| "Winner: Bot " ++ String.fromInt winner

        _ ->
            none


updateElt : Int -> (a -> a) -> Array a -> Array a
updateElt idx updfn array =
    case A.get idx array of
        Just item ->
            A.set idx (updfn item) array

        Nothing ->
            array


collideArray : Array Bot -> Array Bot
collideArray bots =
    let
        cm1 =
            A.length bots - 1
    in
    List.foldr
        (\i1 bots1 ->
            List.foldr
                (\i2 bots2 ->
                    case ( A.get i1 bots2, A.get i2 bots2 ) of
                        ( Just b1, Just b2 ) ->
                            case collide b1 b2 of
                                Just ( c1, c2 ) ->
                                    bots2
                                        |> A.set i1 c1
                                        |> A.set i2 c2

                                Nothing ->
                                    bots2

                        _ ->
                            bots2
                )
                bots1
                (List.range (i1 + 1) cm1)
        )
        bots
        (List.range 0 (cm1 - 1))


collideD2 =
    (2 * botRadius) ^ 2


collide : Bot -> Bot -> Maybe ( Bot, Bot )
collide b1 b2 =
    if b1.dead || b2.dead then
        Just ( b1, b2 )

    else
        let
            ( x1, y1 ) =
                b1.position

            ( x2, y2 ) =
                b2.position

            dx =
                x2 - x1

            dy =
                y2 - y1

            d2 =
                dx * dx + dy * dy
        in
        if d2 > collideD2 then
            Nothing

        else
            let
                d =
                    sqrt d2

                ux =
                    dx / d

                uy =
                    dy / d

                ( v1, v2 ) =
                    velCollide ( b1.velocity, b2.velocity ) ( ux, uy )
            in
            Just ( { b1 | velocity = v1 }, { b2 | velocity = v2 } )


velCollide : ( Vec, Vec ) -> Vec -> ( Vec, Vec )
velCollide ( ( v1x, v1y ), ( v2x, v2y ) ) ( ux, uy ) =
    let
        proj1 =
            v1x * ux + v1y * uy

        ( fb1x, fb1y ) =
            ( ux * proj1, uy * proj1 )

        proj2 =
            v2x * -ux + v2y * -uy

        ( fb2x, fb2y ) =
            ( -ux * proj2, -uy * proj2 )

        v1 =
            ( v1x - fb1x + fb2x, v1y - fb1y + fb2y )

        v2 =
            ( v2x - fb2x + fb1x, v2y - fb2y + fb1y )
    in
    ( v1, v2 )


isDead : Vec -> Bool
isDead ( x, y ) =
    let
        lower =
            -1 + botRadius

        upper =
            1 - botRadius
    in
    x < lower || x > upper || y < lower || y > upper



-- in range?


updateUrl : Model -> Cmd Msg
updateUrl model =
    BN.replaceUrl model.navkey (makeUrl model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgramTextChanged idx txt ->
            case A.get idx model.bots of
                Just bot ->
                    let
                        nmodel =
                            { model | bots = A.set idx { bot | programText = txt, program = Err "uncompiled" } model.bots }
                    in
                    ( nmodel
                    , updateUrl nmodel
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddBot ->
            let
                nmodel =
                    { model | bots = defaultBotPositions botSpawnRadius <| A.push emptyBot model.bots }
            in
            ( nmodel
            , updateUrl nmodel
            )

        DeleteBot idx ->
            let
                bl =
                    A.toList model.bots

                nmodel =
                    { model
                        | bots = A.fromList (List.take idx bl ++ List.drop (idx + 1) bl)
                    }
            in
            ( nmodel
            , updateUrl nmodel
            )

        Stop ->
            ( { model | go = False }, Cmd.none )

        Sumo on ->
            ( { model
                | sumo = on
              }
            , Cmd.none
            )

        ShowCode on ->
            ( { model | showCode = on }, Cmd.none )

        AniFrame millis ->
            let
                botControl =
                    BotControl { botidx = 0, bots = model.bots, prints = model.prints }

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
                            if bot.dead then
                                bot

                            else
                                let
                                    vel =
                                        vecPlus bot.velocity bot.accel

                                    pos =
                                        vecPlus bot.position vel

                                    dead =
                                        if model.sumo then
                                            isDead pos

                                        else
                                            False
                                in
                                { bot
                                    | velocity = vel
                                    , position = pos
                                    , dead = dead
                                    , step =
                                        if dead then
                                            EbError "dead"

                                        else
                                            bot.step
                                }
                        )
                        nbc.bots
            in
            ( { model
                | bots = collideArray nbots
                , prints = nbc.prints
              }
            , Cmd.none
            )

        Go ->
            ( { model
                | go = True -- have to do this here because of https://github.com/elm/compiler/issues/1776
              }
            , Random.generate RandomBPs <| RL.shuffle (botPositions 0.5 (A.length model.bots))
            )

        RandomBPs ps ->
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
                                                EbStart botlang (BotControl { botidx = idx, bots = model.bots, prints = Dict.empty }) prog
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
                | bots = unDead <| applyBotPositions (A.fromList ps) compiledBots
                , prints = Dict.empty
                , go = True
              }
            , Cmd.none
            )

        OnUrlRequest rq ->
            ( model, Cmd.none )

        OnUrlChange url ->
            ( model, Cmd.none )

        RightPanelViewSelected rpv ->
            ( { model | rightPanelView = rpv }, Cmd.none )

        ShowBotFtns v ->
            ( { model | showBotFtns = v }, Cmd.none )

        ShowPreludeFtns v ->
            ( { model | showPreludeFtns = v }, Cmd.none )


main =
    Browser.application
        { init = init
        , view = \model -> { title = "schelme bots", body = [ layout [] <| view model ] }
        , update = update
        , subscriptions =
            \model ->
                case model.go of
                    True ->
                        BE.onAnimationFrameDelta AniFrame

                    False ->
                        Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
