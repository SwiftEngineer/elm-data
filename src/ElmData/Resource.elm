module ElmData.Resource exposing (..)

import ElmData.Data exposing (..)

import Http

type alias Resource recordType externalMsg =
    { create : (recordType -> Cmd externalMsg)
    , fetch : (String -> Cmd externalMsg)
    , update : (recordType -> Cmd externalMsg)
    }

type ResourceMsg recordType
    = Success recordType
    | Failure RequestError

resource : DAO recordType externalMsg -> (ResourceMsg recordType -> localMsg) -> (localMsg -> externalMsg) -> Resource recordType externalMsg
resource dao resourceToLocal localToExternal =
    { create = curryPost dao (createResourceToExternalMsgTranslation resourceToLocal localToExternal)
    , fetch = curryFetch dao (createResourceToExternalMsgTranslation resourceToLocal localToExternal)
    , update = curryPut dao (createResourceToExternalMsgTranslation resourceToLocal localToExternal)
    }

createResourceToExternalMsgTranslation : (ResourceMsg recordType -> localMsg) -> (localMsg -> externalMsg) -> (RequestResult recordType -> externalMsg)
createResourceToExternalMsgTranslation resourceToLocal localToExternal =
    \requestResults ->
        case requestResults of
            Result.Ok response ->
                Success response.body |> resourceToLocal |> localToExternal
            Result.Err error ->
                Failure error |> resourceToLocal |> localToExternal