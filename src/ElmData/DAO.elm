module ElmData.DAO exposing (DAO, createDAO, update, DAOEvent(..), DAOMsg(..))

{-|
    DAOs (Data Access Objects) exist to hold all state relevant to making requests to an API.

    @docs DAO, createDAO, update, DAOEvent, DAOMsg
-}

import Json.Decode
import Json.Encode

import ElmData.Messages exposing (..)


{-|
    the dao
-}
type alias DAO recordType msg =
    { apiUrl : String
    , authToken : String

    -- message passing
    , msgTranslator : (DAOMsg recordType msg -> msg)

    -- session renewal
    , daoEventToMessage : (DAOEvent recordType msg -> msg)

    -- serialization
    , listDeserialize : Json.Decode.Decoder (List recordType)
    , deserialize : Json.Decode.Decoder recordType
    , serialize : (recordType -> Json.Encode.Value)
    }

{-|
    Function used to create a DAO
-}
createDAO : String -> String -> (DAOMsg recordType msg -> msg) -> (DAOEvent recordType msg -> msg) -> Json.Decode.Decoder (List recordType) -> Json.Decode.Decoder recordType -> (recordType -> Json.Encode.Value) -> (DAO recordType msg)
createDAO apiUrl authToken msgTranslator daoEventToMessage listDeserializer deserializer serializer =
    { apiUrl = apiUrl
    , authToken = authToken

    -- message passing
    , msgTranslator = msgTranslator

    -- session renewal
    , daoEventToMessage = daoEventToMessage

    -- serialization
    , listDeserialize = listDeserializer
    , deserialize = deserializer
    , serialize = serializer
    }


-- UPDATE

{-|
    Function used to update a DAO
-}
update : DAO recordType msg -> DAOMsg recordType msg -> (DAO recordType msg, msg)
update dao msg =
    case msg of
        RequestResultMsg res ret ->
            case res of
                Result.Ok _ ->
                    (dao, dao.daoEventToMessage <| DAORequestSuccess dao msg ret)
                Result.Err err ->
                    (dao, dao.daoEventToMessage <| DAORequestFailure dao err ret)
        RequestResultsMsg res ret ->
            case res of
                Result.Ok _ ->
                    (dao, dao.daoEventToMessage <| DAORequestSuccess dao msg ret)
                Result.Err err ->
                    (dao, dao.daoEventToMessage <| DAORequestFailure dao err ret)


{-|
    a DAO event
-}
type DAOEvent recordType msg
    = DAORequestSuccess (DAO recordType msg) (DAOMsg recordType msg) msg
    | DAORequestFailure (DAO recordType msg) RequestError msg


-- TODO remove as it is confusing that both it and events exist
{-|
    a DAO message
-}
type DAOMsg recordType msg
    = RequestResultMsg (RequestResult recordType) msg
    | RequestResultsMsg (RequestResults recordType) msg