module ElmData.Messages exposing (RequestError, RequestResult, RequestResults, DAORequestResponse, ListDAORequestResponse)

import Http exposing (Response)

type RequestError
    = UnableToParseResponseBody String
    | HttpError Http.Error

type alias RequestResult recordType = (Result RequestError (DAORequestResponse recordType))
type alias RequestResults recordType = (Result RequestError (ListDAORequestResponse recordType))

-- MODELS

type alias DAORequestResponse recordType =
    { body : recordType
    }

type alias ListDAORequestResponse recordType =
    { body : List recordType
    }