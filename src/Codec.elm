module Codec exposing(encodeNoteListAsString)

import Json.Encode as E
import Json.Decode as D
import Time exposing (utc, toHour, toMinute, toSecond)
import Note exposing(Note)

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
    , ("timeCreated", E.string (toUtcString note.timeCreated))
    , ("timeModified", E.string (toUtcString note.timeModified))
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
