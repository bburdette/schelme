module Main exposing (Model, Msg(..), main, view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import Prelude


type Msg
    = ProgramTextChanged String
    | Eval


type alias Model =
    { programText : String
    , programOutput : Result String String
    , finalNamespace : Eval.NameSpace ()
    }


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


init =
    { programText = """(defn (test a b) (+ a b))
(def x 123)
(def y 456)
(test x y)"""
    , programOutput = Ok ""
    , finalNamespace = Dict.empty
    }


viewNamespace : Eval.NameSpace () -> Element Msg
viewNamespace ns =
    column [ width fill ] <|
        List.map
            (\( name, term ) ->
                row [ width fill, spacing 7 ] [ el [ width fill ] <| text name, el [ width fill ] <| text <| Eval.showTerm term ]
            )
            (Dict.toList ns)


view : Model -> Element Msg
view model =
    column [ width fill ]
        [ row [ width fill ]
            [ EI.multiline [ width fill, height shrink ]
                { onChange = ProgramTextChanged
                , text = model.programText
                , placeholder = Nothing
                , label = EI.labelAbove [] <| text "schelme code here: "
                , spellcheck = False
                }
            , column [ width fill ] [ el [ Font.bold ] <| text "initial namespace", viewNamespace Prelude.prelude ]
            ]
        , EI.button buttonStyle
            { onPress = Just Eval
            , label = text "Eval"
            }
        , row [ width fill ]
            [ column [ width fill, alignTop ]
                [ el [ Font.bold ] <| text "Program final output"
                , case model.programOutput of
                    Err e ->
                        el [ Font.color <| rgb255 204 0 0 ] <| text e

                    Ok t ->
                        el [ Font.color <| rgb255 115 210 22 ] <| text t
                ]
            , column [ width fill ] [ el [ Font.bold ] <| text "final namespace", viewNamespace model.finalNamespace ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextChanged txt ->
            { model | programText = txt }

        Eval ->
            let
                rs =
                    Eval.compile model.programText
                        |> Result.andThen
                            (\prog ->
                                Eval.run prog ( Prelude.prelude, () )
                                    |> Result.andThen
                                        (\( ns, term ) ->
                                            Ok <| ( ns, Eval.showTerm term )
                                        )
                            )
            in
            case rs of
                Ok ( finalns, output ) ->
                    { model
                        | programOutput = Ok output
                        , finalNamespace = Tuple.first finalns
                    }

                Err e ->
                    { model
                        | programOutput = Err e
                        , finalNamespace = Dict.empty
                    }


main =
    Browser.sandbox
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        }
