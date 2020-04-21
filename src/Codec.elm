module Codec exposing(encodeNoteListAsString, decodeNoteList)

import Json.Encode as E
import Json.Decode as D
import Time exposing (utc, toHour, toMinute, toSecond)
import Note exposing(Note)


{- ENCODERS -}

encodeNoteListAsString : List Note -> String
encodeNoteListAsString noteList =
  E.encode 4 (encodeNoteList noteList)


encodeNoteList : List Note -> E.Value
encodeNoteList noteList =
  E.list encodeNote noteList

encodeNote : Note -> E.Value
encodeNote note =
  E.object [
     ("id", E.string note.id)
    ,("subject", E.string note.subject)
    , ("body", E.string note.body)
    , ("tags", E.list  E.string note.tags)
    , ("timeCreated", E.int (Time.posixToMillis note.timeCreated))
    , ("timeModified", E.int (Time.posixToMillis note.timeModified))
    , ("selected", E.bool note.selected)
  ]



toUtcString : Time.Posix -> String
toUtcString time =
  String.fromInt (toHour utc time)
  ++ ":" ++
  String.fromInt (toMinute utc time)
  ++ ":" ++
  String.fromInt (toSecond utc time)
  ++ " (UTC)"

{-| DECODERS -}


decodeNoteList : String -> Result D.Error (List Note)
decodeNoteList data =
  D.decodeString noteListDecoder data

noteListDecoder : D.Decoder (List Note)
noteListDecoder =
  D.list noteDecoder

noteDecoder : D.Decoder Note
noteDecoder =
  D.map7 Note
    (D.field "id" D.string)
    (D.field "subject" D.string)
    (D.field "tags" (D.list D.string))
    (D.field "body" D.string)
    (D.field "timeCreated" (D.int |> D.map Time.millisToPosix))
    (D.field "timeModified" (D.int |> D.map Time.millisToPosix))
    (D.field "selected" (D.bool ))
