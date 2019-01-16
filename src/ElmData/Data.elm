module ElmData.Data exposing (..)

{-|
    Main module for DAO related things.
-}

import Http exposing (Response)

import Json.Decode
import Json.Encode

import ElmData.QueryParam exposing (..)
import ElmData.Messages exposing (..)
import ElmData.DAO exposing (..)


-- Creates a function that will create and send requests for a given record.
--
--curryFetch : String -> String -> (DAOMsg recordType -> msg) -> Json.Decode.Decoder recordType -> (String -> Cmd msg)
curryFetch : DAO recordType msg -> (RequestResult recordType -> msg) -> (String -> Cmd msg)
curryFetch dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))


        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request
        createRequestForSingleRecord identifier = Http.request
            { method = "GET"
            , headers = 
                [ Http.header "Authorization" dao.authToken
                ]
            , url = (dao.apiUrl ++ "/" ++ identifier)
            , body = Http.emptyBody
            , expect = Http.expectStringResponse <| createResponseExpectation dao.deserialize
            , timeout = Nothing
            , withCredentials = False
            }

        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        requestPromise identifier = Http.send requestHandler (createRequestForSingleRecord identifier)
    in
        -- put it all together to create a function that creates THEN sends a request for a given record
        \identifier -> requestPromise identifier


-- Creates a function that will send request to delete a given record.
--
curryDelete : DAO recordType msg -> (RequestResults recordType -> msg) -> (String -> Cmd msg)
curryDelete dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultsMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))


        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request
        createRequestForSingleRecord identifier = Http.request
            { method = "DELETE"
            , headers = 
                [ Http.header "Authorization" dao.authToken
                ]
            , url = (dao.apiUrl ++ "/" ++ identifier)
            , body = Http.emptyBody
            , expect = Http.expectStringResponse <| createEmptyResponseExpectation
            , timeout = Nothing
            , withCredentials = False
            }

        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        requestPromise identifier = Http.send requestHandler (createRequestForSingleRecord identifier)
    in
        -- put it all together to create a function that creates THEN sends a request for a given record
        \identifier -> requestPromise identifier


-- Creates a function that will create and send requests for records based on a query.
--
--curryQuery : String -> String -> (DAOMsg recordType -> msg) -> Json.Decode.Decoder (List recordType) -> (List QueryParam -> Cmd msg)
curryQuery : DAO recordType msg -> (RequestResults recordType -> msg) -> (List QueryParam -> Cmd msg)
curryQuery dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultsMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))

        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request
        createRequestToQuery queryParams = Http.request
            { method = "GET"
            , headers = 
                [ Http.header "Authorization" dao.authToken
                ]
            , url = (createUrl dao.apiUrl queryParams)
            , body = Http.emptyBody
            , expect = Http.expectStringResponse <| createListResponseExpectation dao.listDeserialize
            , timeout = Nothing
            , withCredentials = False
            }

        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        requestPromise queryParams = Http.send requestHandler (createRequestToQuery queryParams)
    in
        -- put it all together to create a function that creates THEN sends a request for a given record
        \queryParams -> requestPromise queryParams

-- Creates a function that will create and send requests for records
--
curryFetchAll : DAO recordType msg -> (RequestResults recordType -> msg) -> Cmd msg
curryFetchAll dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultsMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))

        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request
        createRequestToQuery = Http.request
            { method = "GET"
            , headers = 
                [ Http.header "Authorization" dao.authToken
                ]
            , url = dao.apiUrl
            , body = Http.emptyBody
            , expect = Http.expectStringResponse <| createListResponseExpectation dao.listDeserialize
            , timeout = Nothing
            , withCredentials = False
            }

    in
        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        Http.send requestHandler createRequestToQuery


-- Creates a function that will create and send requests to persist a given record.
--
curryPost : DAO recordType msg -> (RequestResult recordType -> msg) -> (recordType -> Cmd msg)
curryPost dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))

        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request based on some record's identifier
--        createRequestToQuery recordToPersist = Http.post dao.apiUrl (Http.jsonBody <| dao.serialize recordToPersist) dao.deserialize
        createRequestToQuery recordToPersist = Http.request
            { method = "POST"
            , headers =
                [ Http.header "Authorization" dao.authToken
                ]
            , url = dao.apiUrl
            , body = Http.jsonBody <| dao.serialize recordToPersist
            , expect = Http.expectStringResponse <| createResponseExpectation dao.deserialize
            , timeout = Nothing
            , withCredentials = False
            }

        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        requestPromise recordToPersist = Http.send requestHandler <| createRequestToQuery recordToPersist
    in
        -- put it all together to create a function that creates THEN sends a request for a given record
        \recordToPersist -> requestPromise recordToPersist

-- Creates a function that will create and send requests to update a given record.
--
curryPut : DAO recordType msg -> (RequestResult recordType -> msg) -> (recordType -> Cmd msg)
curryPut dao returnMsg =
    let
        -- this will statement wraps the response msg with a dao specific one,
        -- this helps insure that all messages will go through a DAO update,
        -- so that we can make sure that all Resources can talk to one another
        -- through their respective DAOs.
        requestHandler result = dao.msgTranslator (RequestResultMsg (mapHttpErrors result) (returnMsg (mapHttpErrors result)))

        -- PAY ATTENTION: The functions above are bullshit, what's more important are these next
        -- two functions below this line. The first one creates requests, the send one sends them.

        -- this is the function that will be used to CREATE a http request based on some record's identifier
        createRequestToQuery recordToPersist = Http.request
            { method = "PUT"
            , headers = []
            , url = dao.apiUrl
            , body = (Http.jsonBody <| dao.serialize recordToPersist)
            , expect = Http.expectStringResponse <| createResponseExpectation dao.deserialize
            , timeout = Nothing
            , withCredentials = False
            }
        
        -- Http.put apiUrl (Http.jsonBody <| recordEncoder recordToPersist) recordDecoder

        -- this is the function that will be used to SEND an Http Request
        -- (notice how we call the method above)
        requestPromise recordToPersist = Http.send requestHandler <| createRequestToQuery recordToPersist
    in
        -- put it all together to create a function that creates THEN sends a request for a given record
        \recordToPersist -> requestPromise recordToPersist


-- HELPERS

createResponseExpectation : Json.Decode.Decoder recordType -> (Response String -> Result String (DAORequestResponse recordType))
createResponseExpectation decoder =
    let
        createResponse decodedResult =
            { body = decodedResult
            }
    in
        \response ->
            Result.map createResponse (Json.Decode.decodeString decoder response.body)
                |> Result.mapError Json.Decode.errorToString

createListResponseExpectation : Json.Decode.Decoder (List recordType) -> (Response String -> Result String (ListDAORequestResponse recordType))
createListResponseExpectation decoder =
    let
        createResponse decodedResults =
            { body = decodedResults
            }
    in
        \response ->
            Result.map createResponse (Json.Decode.decodeString decoder response.body)
                |> Result.mapError Json.Decode.errorToString

createEmptyResponseExpectation : (Response String -> Result String (ListDAORequestResponse recordType))
createEmptyResponseExpectation =
    let
        createResponse =
            { body = []
            }
    in
        \response ->
            Ok createResponse

mapHttpErrors : Result Http.Error a -> Result RequestError a
mapHttpErrors httpResult =
    Result.mapError HttpError httpResult

requestErrorToString : RequestError -> String
requestErrorToString err =
    case err of
        UnableToParseResponseBody errString ->
            errString
        HttpError httpErr ->
            "HttpError"