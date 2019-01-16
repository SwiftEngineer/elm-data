module ElmData.Session exposing (..)

import Http exposing (..)
import Jwt exposing (..)

import Json.Decode
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as E exposing (Value)

import Task exposing (perform, succeed)

type alias SessionData =
  { authToken : String
  , userName : String
  , email : Maybe String
  , expiration : Float
  , isRegistered : Bool
  }

tokenStringDecoder : Json.Decode.Decoder String
tokenStringDecoder =
    Json.Decode.field "token" Json.Decode.string

type alias SessionHelperConfig msg =
    { apiUrl : String
    , responseMessage : ((Result Http.Error String) -> msg)
    , sessionCreationResultConsumer : (SessionCreationResult -> msg)
    }

-- Initialization --

createConfiguration : String -> (SessionCreationResult -> msg) -> SessionHelperConfig msg
createConfiguration apiUrl sessionCreationResultConsumer =
    let
        responseMessageConsumer responseMessage = handleJwtRequestResponse responseMessage |> sessionCreationResultConsumer
    in
        { apiUrl = apiUrl
        , responseMessage = responseMessageConsumer
        , sessionCreationResultConsumer = sessionCreationResultConsumer
        }

-- Authentication Methods --

authenticateAnonymously : SessionHelperConfig msg -> Cmd msg
authenticateAnonymously conf =
    let
        apiUrl = conf.apiUrl ++ "/sessions"

        creds =
            [] |> E.object |> Http.jsonBody
    in
        Http.post apiUrl creds tokenStringDecoder
            |> Http.send conf.responseMessage

authenticateByUsername : SessionHelperConfig msg -> String -> Cmd msg
authenticateByUsername conf userName =
    let
        apiUrl = conf.apiUrl ++ "/sessions"

        creds =
            [ ( "username", E.string userName )
            ]
                |> E.object
                |> Http.jsonBody
    in
        Http.post apiUrl creds tokenStringDecoder
            |> Http.send conf.responseMessage

type alias LoginCredentials =
    { email : String
    , password : String
    }

authenticateByCredentials : SessionHelperConfig msg -> LoginCredentials -> Cmd msg
authenticateByCredentials conf credentials =
    let
        apiUrl = conf.apiUrl ++ "/sessions"

        creds =
            [ ( "email", E.string credentials.email )
            , ( "password", E.string credentials.password )
            ]
                |> E.object
                |> Http.jsonBody
    in
        Http.post apiUrl creds tokenStringDecoder
            |> Http.send conf.responseMessage


authenticateByToken : SessionHelperConfig msg -> String -> Cmd msg
authenticateByToken conf authToken =
    commandify <| conf.sessionCreationResultConsumer <| sessionFromToken authToken

-- authentication renewal

renewSession : SessionHelperConfig msg -> String -> Cmd msg
renewSession conf authToken =
    let
        apiUrl = conf.apiUrl ++ "/renew"

        creds =
            [ ( "token", E.string authToken )
            ]
                |> E.object
                |> Http.jsonBody
    in
        Http.post apiUrl creds tokenStringDecoder
            |> Http.send conf.responseMessage

-- registration

type alias RegisterCredentials =
   { email : String
   , username : String
   , password : String
   }

authenticateByRegistration : SessionHelperConfig msg -> RegisterCredentials -> Cmd msg
authenticateByRegistration conf credentials =
    let
        apiUrl = conf.apiUrl ++ "/register"

        creds =
            [ ( "email", E.string credentials.email )
            , ( "username", E.string credentials.username )
            , ( "password", E.string credentials.password )
            ]
                |> E.object
                |> Http.jsonBody
    in
        Http.post apiUrl creds tokenStringDecoder
            |> Http.send conf.responseMessage

-- Jwt --

handleJwtRequestResponse : (Result Http.Error String) -> SessionCreationResult
handleJwtRequestResponse response =
    case response of
        Ok token ->
            sessionFromToken token
        Err err ->
            case err of
                BadUrl badUrl ->
                    Failure "The server appears to be offline!"
                Timeout ->
                    Failure "Request to authenticate took too long! Please try again later!"
                NetworkError ->
                    Failure "Couldn't connect. Are you sure you have internet right now?"
                BadStatus badResponse ->
                    case badResponse.status.code of
                        403 ->
                            Failure "Failed to connect, please try again later!"
                        _ ->
                            Failure "Unable to authenticate with the provided credentials!"
                BadPayload payload badResponse ->
                    Failure "Our app is being funky. Please refresh the page before trying again!"


type alias JwtClaims =
    { iss : String
    , exp : Float
    , nbf : Float
    , iat : Float
    , jti : String
    , nme : String
    , eml : Maybe String
    }    

type SessionCreationResult
    = Success SessionData
    | Failure String
    | CorruptSession

sessionFromClaims : String -> JwtClaims -> SessionData
sessionFromClaims authToken claims =
    { authToken = authToken
    , userName = claims.nme
    , email = claims.eml
    , expiration = claims.exp
    , isRegistered = Maybe.map (\eml -> not <| String.isEmpty eml) claims.eml |> Maybe.withDefault False
    }

sessionFromToken : String -> SessionCreationResult
sessionFromToken authToken =
    let
        claimsDecoder = 
            Json.Decode.succeed JwtClaims
                |> required "iss" Json.Decode.string
                |> required "exp" Json.Decode.float
                |> required "nbf" Json.Decode.float
                |> required "iat" Json.Decode.float
                |> required "jti" Json.Decode.string
                |> required "nme" Json.Decode.string
                |> optional "eml" (Json.Decode.nullable Json.Decode.string) Nothing
    in
        case Jwt.decodeToken claimsDecoder authToken of
                Ok claims ->
                    Success <| sessionFromClaims authToken claims
                Err jwtErr ->
                    case jwtErr of
                        TokenDecodeError decodeError ->
                            CorruptSession
                        _ ->
                            Failure "Session was invalid"


commandify : msg -> Cmd msg
commandify msg =
    Task.succeed msg |> Task.perform identity