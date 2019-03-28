module Main exposing (Model, Msg(..), main, view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import EvalStep exposing (EvalBodyStep(..), NameSpace, Term(..))
import Prelude as Prelude exposing (evalArgsSideEffector)
import Run exposing (compile, runCount)
import Show exposing (showTerm)
import Svg as S
import Svg.Attributes as SA


type Msg
    = ProgramTextChanged Int String
    | AddBot
    | Stop
    | Go


type alias Color =
    ( Float, Float, Float )


type alias BotControl =
    { accel : ( Float, Float )
    }


type alias Bot =
    { programText : String
    , program : Result String (List (Term BotControl))
    , color : Color
    , botControl : BotControl
    }


type alias Model =
    { bots : List Bot
    , evalsPerTurn : Int
    }


emptyBot : Bot
emptyBot =
    { programText = ""
    , program = Err "uncompiled"
    , color = ( 1, 1, 1 )
    , botControl = { accel = ( 0, 0 ) }
    }


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]



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


init =
    { bots = []
    , evalsPerTurn = 100
    }


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
    column [ width fill ]
        [ EI.multiline [ width fill, height shrink, alignTop ]
            { onChange = ProgramTextChanged idx
            , text = bot.programText
            , placeholder = Nothing
            , label = EI.labelAbove [ Font.bold ] <| text "schelme code here: "
            , spellcheck = False
            }
        ]


drawBots : List Bot -> Element Msg
drawBots bots =
    el [ width fill, height fill ] <|
        html <|
            S.svg [ SA.width "500", SA.height "500", SA.viewBox "0 0 500 500" ]
                [ S.circle [ SA.cx "100", SA.cy "100", SA.r "20" ] [] ]


view : Model -> Element Msg
view model =
    row [ width fill ]
        [ column [ width fill ]
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
        , drawBots model.bots
        ]



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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextChanged idx txt ->
            model

        AddBot ->
            { model | bots = emptyBot :: model.bots }

        Stop ->
            model

        Go ->
            let
                compiledBots =
                    List.map
                        (\bot ->
                            { bot | program = compile bot.programText }
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
                            compiledBots
                        )
            in
            { model | bots = compiledBots }


main =
    Browser.sandbox
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        }
