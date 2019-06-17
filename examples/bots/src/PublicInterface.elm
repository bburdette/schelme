module PublicInterface exposing (SendMsg(..), ServerResponse(..), encodeSendMsg, serverResponseDecoder)

import Json.Decode as JD
import Json.Encode as JE


type SendMsg
    = GetScript String
    | SaveScript String String
    | GetScriptList


type ServerResponse
    = ServerError String
    | ScriptReceived String String
    | ScriptListReceived (List String)


encodeSendMsg : SendMsg -> JE.Value
encodeSendMsg sm =
    case sm of
        SaveScript name script ->
            JE.object
                [ ( "what", JE.string "savescript" )
                , ( "data"
                  , JE.object
                        [ ( "script", JE.string script )
                        , ( "name", JE.string name )
                        ]
                  )
                ]

        GetScript name ->
            JE.object
                [ ( "what", JE.string "getscript" )
                , ( "data", JE.string name )
                ]

        GetScriptList ->
            JE.object
                [ ( "what", JE.string "getscriptlist" )

                --              , ( "data", JE.string name )
                ]


serverResponseDecoder : JD.Decoder ServerResponse
serverResponseDecoder =
    JD.at [ "what" ] JD.string
        |> JD.andThen
            (\what ->
                case what of
                    "script" ->
                        JD.field "content"
                            (JD.map2 ScriptReceived
                                (JD.field "name" JD.string)
                                (JD.field "script" JD.string)
                            )

                    "scriptlist" ->
                        JD.field "content"
                            (JD.map ScriptListReceived
                                (JD.list JD.string)
                            )

                    wat ->
                        JD.succeed
                            (ServerError ("invalid 'what' from server: " ++ wat))
            )
