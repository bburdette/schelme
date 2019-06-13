module Main exposing (Model, Msg(..), main, view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import EvalStep exposing (NameSpace, Term(..))
import Prelude as Prelude exposing (Reference, TermReference, evalArgsSideEffector)
import Run exposing (compile, runCount)
import Show exposing (showTerm)


type Msg
    = ProgramTextChanged String
    | Eval


type alias Color =
    ( Float, Float, Float )


type alias Model =
    { programText : String
    , programOutput : Result String String
    , finalNamespace : NameSpace Color
    , count : Int
    , color : Color
    }


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


setColor : Prelude.SideEffectorFn Color
setColor ns a argterms =
    case argterms of
        [ TNumber r, TNumber g, TNumber b ] ->
            Ok ( ns, ( r, g, b ), TList [] )

        _ ->
            Err (String.concat ("setColor args should be 3 numbers!  " :: List.map showTerm argterms))


preludeNColor : NameSpace Color
preludeNColor =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.insert "setColor"
            (TSideEffector (evalArgsSideEffector setColor))


reference : Reference
reference =
    Prelude.preludeReference
        |> Dict.union Prelude.mathReference
        |> Dict.insert "setColor"
            (TermReference "(setColor <num1> <num2> <num3>) -> ()" "has the side effect of setting the color of a thing, someplace.")


pg1 =
    """(defn (test a b) (+ a b))
(def x 123)
(defn (setRed r) (setColor 0.1 0.1 r))
(def y 456)
(test x y)
(setRed 0.7)"""


pg2 =
    """(and true false)"""


pg3 =
    """(defn (meh a b) (+ a b))
(meh 6 6)"""


pg4 =
    "(if true 1 2)"


pg5 =
    """(def x 5)
x"""


pg6 =
    """(defn (test a b) (+ a b))
(test 1 1)"""


pg7 =
    """(defn (test a b c) (if (eq c 0) a (test (+ a b) b (- c 1))))
(test 1 1 5)"""


pg8 =
    """(defn (test c) (if (eq c 0) 1 (test 0)))
(test 1)"""


pg9 =
    """(defn (test c) (if (eq c 0) 1 (test (- c 1))))
(test 2)"""


pg10 =
    """(defn (test a b c) 
  (if (eq c 0) 
    a 
    (test (+ a b) b (- c 1))))

(def x (test 1 1 5))
(setColor 0.5 0.5 0.5)
x"""


init : Model
init =
    { programText = pg10
    , programOutput = Ok ""
    , finalNamespace = Dict.empty
    , count = 0
    , color = ( 1, 1, 1 )
    }


viewNamespace : NameSpace a -> Element Msg
viewNamespace ns =
    column [ width fill ] <|
        List.map
            (\( name, term ) ->
                row [ width fill, spacing 7 ] [ el [ width fill ] <| text name, el [ width fill ] <| text <| showTerm term ]
            )
            (Dict.toList ns)


viewReference : Reference -> Element Msg
viewReference ref =
    column [ width fill, spacing 7, scrollbarY, height (px 300) ] <|
        List.map
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
    let
        ( r, g, b ) =
            model.color
    in
    column [ width fill ]
        [ row [ width fill ]
            [ column [ width fill ]
                [ EI.multiline [ width fill, height shrink, alignTop ]
                    { onChange = ProgramTextChanged
                    , text = model.programText
                    , placeholder = Nothing
                    , label = EI.labelAbove [ Font.bold ] <| text "schelme code here: "
                    , spellcheck = False
                    }
                , el [ Font.bold ] <| text "language reference"
                , viewReference reference
                ]
            , column [ width fill ]
                [ el [ Font.bold ] <| text "initial namespace"
                , viewNamespace preludeNColor
                ]
            ]
        , EI.button buttonStyle
            { onPress = Just Eval
            , label = text "Eval"
            }
        , row [ width fill ]
            [ column [ width fill, alignTop, spacing 5 ]
                [ el [ Font.bold ] <| text "Program output:"
                , case model.programOutput of
                    Err e ->
                        paragraph [ Font.color <| rgb255 204 0 0 ] [ text e ]

                    Ok t ->
                        paragraph [ Font.color <| rgb255 115 210 22 ] [ text t ]
                , el [ Font.bold ] <| text "Step count: "

                -- , text <| String.fromInt (stepmax - model.count)
                , text <| String.fromInt model.count
                , el [ Font.bold ] <| text "Side effect color:"
                , el [ width (px 150), height (px 150), Background.color (rgb r g b) ] <| text "    "
                ]
            , column [ width fill, alignTop ] [ el [ Font.bold ] <| text "final namespace", viewNamespace model.finalNamespace ]
            ]
        ]


stepmax : Int
stepmax =
    500


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextChanged txt ->
            { model | programText = txt }

        Eval ->
            let
                rs =
                    compile model.programText
                        |> Result.andThen
                            (\prog ->
                                -- Eval.runLimit preludeNColor model.color stepmax prog
                                runCount preludeNColor model.color prog
                            )
            in
            case rs of
                Ok ( finalns, color, ( count, output ) ) ->
                    { model
                        | programOutput = Ok (showTerm output)
                        , finalNamespace = finalns
                        , count = count
                        , color = color
                    }

                Err e ->
                    { model
                        | programOutput = Err e
                        , finalNamespace = Dict.empty
                        , count = 0
                        , color = ( 1, 1, 1 )
                    }


main =
    Browser.sandbox
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        }
