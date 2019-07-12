module ElmData.Session exposing (Session(..), SessionData, sessionFromToken, SessionCreationResult)

{-|
    Sessions exist to hold all state associated with your requests.

    Right now that's just Auth data, but I think this could also be a great place for cached data to live as well.

    @docs Session, SessionData, sessionFromToken, SessionCreationResult
-}

import Http exposing (..)
import Jwt exposing (..)

import Json.Decode
import Json.Decode.Pipeline exposing (required)


{-| The Session
-}
type Session
    = Unauthenticated
    | Active SessionData

{-| Data related to an authenticated session
-}
type alias SessionData =
  { authToken : String
  , expiration : Float
  }

{-| Basic Jwt Claims. In the future this should be configurable.
-}
type alias JwtClaims =
    { iss : String
    , exp : Float
    }

-- Session Creation --

{-| Result of the Creation of a Session
-}
type SessionCreationResult
    = Success Session
    | Failure String
    | Corrupt
    | ExpiredSession

-- helper used to map jwt claims into session data
sessionFromClaims : String -> JwtClaims -> SessionData
sessionFromClaims authToken claims =
    { authToken = authToken
    , expiration = claims.exp
    }


{-| Used to attempt to create a Session from a JWT
-}
sessionFromToken : String -> SessionCreationResult
sessionFromToken authToken =
    let
        claimsDecoder = 
            Json.Decode.succeed JwtClaims
                |> required "iss" Json.Decode.string
                |> required "exp" Json.Decode.float
    in
        case Jwt.decodeToken claimsDecoder authToken of
                Ok claims ->
                    Success <| Active <| sessionFromClaims authToken claims
                Err jwtErr ->
                    case jwtErr of
                        TokenDecodeError _ ->
                            Corrupt
                        TokenExpired ->
                            ExpiredSession
                        _ ->
                            Failure "Session was invalid"



-- Login over Http --

tokenStringDecoder : Json.Decode.Decoder String
tokenStringDecoder =
    Json.Decode.field "token" Json.Decode.string

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