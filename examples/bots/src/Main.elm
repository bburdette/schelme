port module Main exposing (main)

import Array as A exposing (Array)
import BotGame exposing (..)
import BotLang exposing (Bot, BotControl(..), allreference, botSpawnRadius, botreference)
import Browser exposing (UrlRequest)
import Browser.Events as BE
import Browser.Navigation as BN exposing (Key)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Element.Input as EI
import EvalStep exposing (EvalBodyStep(..), NameSpace, Term(..))
import Html.Attributes as HA
import Http
import Json.Encode as JE
import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=))
import Prelude as Prelude
import PublicInterface as PI
import Random
import Random.List as RL
import SelectString
import Show exposing (showTerm)
import Url exposing (Url)


port storeLocalVal : ( String, String ) -> Cmd msg


port getLocalVal : ( String, String ) -> Cmd msg


port clearLocalStorage : () -> Cmd msg


port localVal : (( String, String, Maybe String ) -> msg) -> Sub msg


type Msg
    = ProgramTextChanged Int String
    | NameChanged Int String
    | AddBot
    | DeleteBot Int
    | GetBot
    | SaveBot Int
    | SelectBot String
    | CancelBotSelect
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
    | ServerResponse (Result Http.Error PI.ServerResponse)
    | LocalVal { what : String, name : String, mbval : Maybe String }
    | SaveHover (Maybe Int)


type RightPanelView
    = Game
    | CommandGlossary


type alias Model =
    { bots : Array Bot
    , prints : Dict Int (List String)
    , evalsPerTurn : Int
    , sumo : Bool
    , navkey : BN.Key
    , go : Bool
    , showCode : Bool
    , rightPanelView : RightPanelView
    , showPreludeFtns : Bool
    , showBotFtns : Bool
    , location : String
    , infront : Maybe (Element Msg)
    , serverbots : List String
    , saveHover : Maybe Int
    }


infrontDialog : m -> Element m -> Element m
infrontDialog cancelmsg elt =
    Element.el
        [ BG.color <| rgba 0 0 0 0.3
        , width fill
        , height fill
        , Element.inFront <| el [ Border.width 4, centerX, centerY ] elt
        ]
    <|
        Element.column
            [ EE.onClick cancelmsg
            , width fill
            , height fill
            ]
            []


storeBots : Model -> Cmd Msg
storeBots model =
    Cmd.batch <|
        [ storeLocalVal
            ( "sumo"
            , if model.sumo then
                "1"

              else
                "0"
            )
        , storeLocalVal
            ( "botcount", String.fromInt (A.length model.bots) )
        ]
            ++ List.indexedMap
                storeBot
                (A.toList model.bots)


storeBot : Int -> Bot -> Cmd Msg
storeBot idx bot =
    Cmd.batch
        [ storeLocalVal ( "botname" ++ String.fromInt idx, bot.name )
        , storeLocalVal ( "botscript" ++ String.fromInt idx, bot.programText )
        ]


restoreBots : Cmd Msg
restoreBots =
    Cmd.batch
        [ getLocalVal ( "restore", "sumo" )
        , getLocalVal ( "restore", "botcount" )
        ]


restoreReceived : String -> String -> Model -> ( Model, Cmd Msg )
restoreReceived name val model =
    case name of
        "sumo" ->
            ( { model
                | sumo =
                    if val == "1" then
                        True

                    else
                        False
              }
            , Cmd.none
            )

        "botcount" ->
            val
                |> String.toInt
                |> Maybe.map
                    (\count ->
                        ( { model | bots = A.repeat count BotGame.emptyBot }
                        , Cmd.batch
                            (List.concat
                                (List.map
                                    (\idx ->
                                        [ getLocalVal ( "restore", "botname" ++ String.fromInt idx )
                                        , getLocalVal ( "restore", "botscript" ++ String.fromInt idx )
                                        ]
                                    )
                                    (List.range 0 count)
                                )
                            )
                        )
                    )
                |> Maybe.withDefault
                    ( model, Cmd.none )

        other ->
            if String.startsWith "botname" other then
                case String.toInt (String.dropLeft 7 other) of
                    Just idx ->
                        ( { model | bots = updateElt idx (\bot -> { bot | name = val }) model.bots }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else if String.startsWith "botscript" other then
                case String.toInt (String.dropLeft 9 other) of
                    Just idx ->
                        ( { model | bots = updateElt idx (\bot -> { bot | programText = val }) model.bots }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ BG.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


type alias Flags =
    { location : String
    , useragent : String
    , width : Int
    , height : Int
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { bots = A.empty
      , evalsPerTurn = 100
      , go = False
      , navkey = key
      , prints = Dict.empty
      , sumo = True
      , showCode = True
      , rightPanelView = Game
      , showPreludeFtns = True
      , showBotFtns = True
      , location = flags.location
      , serverbots = []
      , infront = Nothing
      , saveHover = Nothing
      }
    , Cmd.batch
        [ mkPublicHttpReq
            flags.location
            PI.GetScriptList
        , restoreBots
        ]
    )


httpErrorString : Http.Error -> String
httpErrorString e =
    case e of
        Http.BadUrl str ->
            "badurl" ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "networkerror"

        Http.BadStatus x ->
            "badstatus: " ++ String.fromInt x

        Http.BadBody r ->
            "badbody\nresponse body: " ++ r


mkPublicHttpReq : String -> PI.SendMsg -> Cmd Msg
mkPublicHttpReq location msg =
    Http.post
        { url = location ++ "/public"
        , body = Http.jsonBody (PI.encodeSendMsg msg)
        , expect = Http.expectJson ServerResponse PI.serverResponseDecoder
        }


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


viewBot : Bool -> Dict Int (List String) -> Bool -> Int -> Bot -> Element Msg
viewBot showCode prints savehover idx bot =
    let
        ( r, g, b ) =
            getBotColor idx
    in
    column [ width fill, Border.widthXY 0 2 ] <|
        [ row [ width fill, spacing 7 ]
            [ el [ Font.bold ] <| text <| "Bot " ++ String.fromInt idx
            , el [ width (px 25), height (px 25), BG.color (rgb r g b) ] <| text "    "
            , EI.text [ width fill, height (maximum 500 shrink), alignTop ]
                { onChange = NameChanged idx
                , text = bot.name
                , placeholder = Nothing
                , label = EI.labelHidden "bot name"
                }
            , EI.button
                ((if savehover then
                    [ below
                        (el [ centerX, paddingXY 0 15 ]
                            (el
                                [ centerX
                                , BG.color <| rgb 0 0 0
                                , paddingXY 10 5
                                , Border.rounded 3
                                ]
                             <|
                                text "save to server"
                            )
                        )
                    ]

                  else
                    []
                 )
                    ++ (EE.onMouseEnter (SaveHover <| Just idx)
                            :: EE.onMouseLeave (SaveHover Nothing)
                            :: alignRight
                            :: buttonStyle
                       )
                )
                { onPress = Just <| SaveBot idx
                , label = text "Save"
                }
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


rda : List (Element.Attribute a)
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
                    , label = text "New Bot"
                    }
                , EI.button buttonStyle
                    { onPress = Just Go
                    , label = text "Go"
                    }
                , EI.button buttonStyle
                    { onPress = Just Stop
                    , label = text "Stop"
                    }
                , EI.button buttonStyle
                    { onPress = Just GetBot
                    , label = text "Get Bot"
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
                :: List.indexedMap (\i b -> viewBot model.showCode model.prints (Just i == model.saveHover) i b) (A.toList model.bots)
        , column [ width fill, alignTop ]
            [ EI.radioRow
                (spacing 5 :: rda)
                { onChange = RightPanelViewSelected
                , options =
                    [ EI.option Game (text "Game")
                    , EI.option CommandGlossary (text "Language Reference")
                    ]
                , selected = Just model.rightPanelView
                , label = EI.labelLeft [ centerY ] <| text "show:"
                }
            , case model.rightPanelView of
                Game ->
                    column [ width fill, alignTop ]
                        [ drawBots model.bots model.sumo
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged idx txt ->
            case A.get idx model.bots of
                Just bot ->
                    let
                        nmodel =
                            { model | bots = A.set idx { bot | name = txt } model.bots }
                    in
                    ( nmodel
                    , storeBots nmodel
                    )

                Nothing ->
                    ( model, Cmd.none )

        ProgramTextChanged idx txt ->
            case A.get idx model.bots of
                Just bot ->
                    let
                        nmodel =
                            { model | bots = A.set idx { bot | programText = txt, program = Err "uncompiled" } model.bots }
                    in
                    ( nmodel
                    , storeBots nmodel
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddBot ->
            let
                nmodel =
                    { model | bots = defaultBotPositions botSpawnRadius <| A.push emptyBot model.bots }
            in
            ( nmodel
            , storeBots nmodel
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
            , storeBots nmodel
            )

        GetBot ->
            ( { model
                | infront =
                    Just <|
                        infrontDialog CancelBotSelect <|
                            SelectString.view "Select a Bot" model.serverbots SelectBot CancelBotSelect
              }
            , Cmd.none
            )

        CancelBotSelect ->
            ( { model | infront = Nothing }, Cmd.none )

        SelectBot name ->
            ( model, mkPublicHttpReq model.location (PI.GetScript name) )

        SaveBot idx ->
            case A.get idx model.bots of
                Just bot ->
                    ( model, mkPublicHttpReq model.location (PI.SaveScript bot.name bot.programText) )

                Nothing ->
                    ( model, Cmd.none )

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
            ( gameStep model millis
            , Cmd.none
            )

        Go ->
            ( { model
                | go = True -- have to do this here because of https://github.com/elm/compiler/issues/1776
              }
            , Random.generate RandomBPs <| RL.shuffle (botPositions 0.5 (A.length model.bots))
            )

        RandomBPs ps ->
            ( assignBotPositions model ps
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

        ServerResponse srr ->
            case srr of
                Ok sr ->
                    case sr of
                        PI.ServerError error ->
                            ( model, Cmd.none )

                        PI.ScriptReceived name script ->
                            let
                                nmodel =
                                    { model
                                        | bots =
                                            defaultBotPositions botSpawnRadius <|
                                                A.push
                                                    { emptyBot
                                                        | name = name
                                                        , programText = script
                                                    }
                                                    model.bots
                                    }
                            in
                            ( nmodel
                            , storeBots nmodel
                            )

                        PI.ScriptListReceived scriptnames ->
                            ( { model | serverbots = scriptnames }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        LocalVal lvdata ->
            case ( lvdata.what, lvdata.mbval ) of
                ( "restore", Just v ) ->
                    restoreReceived lvdata.name v model

                _ ->
                    ( model, Cmd.none )

        SaveHover mbidx ->
            ( { model | saveHover = mbidx }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "schelme bots"
                , body =
                    [ layout
                        (model.infront
                            |> Maybe.map (\x -> [ inFront x ])
                            |> Maybe.withDefault []
                        )
                      <|
                        view model
                    ]
                }
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ case model.go of
                        True ->
                            BE.onAnimationFrameDelta AniFrame

                        False ->
                            Sub.none
                    , localVal (\( w, n, mbv ) -> LocalVal { what = w, name = n, mbval = mbv })
                    ]
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
