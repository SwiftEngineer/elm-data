module ElmData.DAO exposing (DAO, createDAO)

{-|
    DAOs (Data Access Objects) exist to hold all state relevant to making requests to an API.

    @docs DAO, createDAO
-}

import Json.Decode
import Json.Encode

{-|
    the dao
-}
type alias DAO recordType =
    { apiUrl : String
    , authToken : String

    -- serialization
    , listDeserialize : Json.Decode.Decoder (List recordType)
    , deserialize : Json.Decode.Decoder recordType
    , serialize : (recordType -> Json.Encode.Value)
    }

{-|
    Function used to create a DAO
-}
createDAO : String -> String -> Json.Decode.Decoder (List recordType) -> Json.Decode.Decoder recordType -> (recordType -> Json.Encode.Value) -> (DAO recordType)
createDAO apiUrl authToken listDeserializer deserializer serializer =
    { apiUrl = apiUrl
    , authToken = authToken

    -- serialization
    , listDeserialize = listDeserializer
    , deserialize = deserializer
    , serialize = serializer
    }