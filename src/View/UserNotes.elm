module View.UserNotes exposing (view)

import Config exposing (config)
import DateTime
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FrequencyDict
import Html exposing (Html)
import Html.Attributes as HA
import Markdown
import Note exposing (Note)
import Style
import Text
import Time exposing (Posix)
import Types exposing (AppMode(..), DeleteNoteSafety(..), FrontendModel, FrontendMsg(..), NotesMode(..), ValidationState(..))
import User exposing (User)
import View.Button
import View.Utility exposing (showIf, hideIf  )


type alias Model =
    FrontendModel


view : Model -> Element FrontendMsg
view model =
    column Style.mainColumnX
        [ filterPanel model
        , row []
            [ noteListPanel model
            , showIf (model.appMode == UserNotes CreatingNote) (newNotePanel model)
            , showIf (model.appMode == UserNotes EditingNote) (editNotePanel model)
            , viewNote model.maybeCurrentNote
            , showIf model.manualVisible manual
            , hideIf (model.manualVisible || model.appMode == UserNotes CreatingNote || model.appMode == UserNotes EditingNote) (tagsView model)
            ]
        ]


newNotePanel model =
    let
        key1 =
            "aa" ++ String.fromInt model.counter

        key2 =
            "bb" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ newNoteButton, inputNewNoteName model ]
        , Keyed.row [] [ ( key1, inputNoteBody model ) ]
        , Keyed.row [] [ ( key2, inputNoteTags model ) ]
        ]


editNotePanel model =
    let
        key1 =
            "a" ++ String.fromInt model.counter

        key2 =
            "aa" ++ String.fromInt model.counter
    in
    column [ spacing 12, paddingXY 20 0 ]
        [ row [ spacing 12 ] [ updateNoteButton, inputChangedSubject model ]
        , Keyed.row [] [ ( key1, inputNoteBody model ) ]
        , Keyed.row [] [ ( key2, inputNoteTags model ) ]
        ]


inputNoteBody model =
    Input.multiline (Style.multiline 400 (round (config.panelHeight - 85)))
        { onChange = GotNoteBody
        , text = model.noteBody
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        , spellcheck = False
        }


deleteNoteControls model =
    row [ spacing 12 ]
        [ View.Button.deleteNote model
        , showIf (model.deleteNoteSafety == DeleteNoteSafetyOff) cancelDeleteNoteButton
        ]


noteControls model =
    row [ spacing 12 ]
        [ el [] (text "noteControls")
        ]


newNoteButton : Element FrontendMsg
newNoteButton =
    Input.button Style.button
        { onPress = Just MakeNewNote
        , label = Element.text "Create note"
        }


updateNoteButton : Element FrontendMsg
updateNoteButton =
    Input.button Style.button
        { onPress = Just FENoop
        , label = Element.text "Subject"
        }


cancelDeleteNoteButton : Element FrontendMsg
cancelDeleteNoteButton =
    Input.button Style.button
        { onPress = Just <| SetDeleteNoteSafety DeleteNoteSafetyOn
        , label = Element.text "Cancel"
        }


inputNewNoteName model =
    Input.text (Style.inputStyle 285)
        { onChange = GotNewNoteName
        , text = model.newSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteTags model =
    Input.text (Style.inputStyle 365)
        { onChange = GotTagString
        , text = model.tagString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "Tags: ")
        }


inputChangedSubject model =
    Input.text (Style.inputStyle 315)
        { onChange = GotChangedSubject
        , text = model.changedSubject
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
--
-- VIEWLOGS
--


noteListPanel : Model -> Element FrontendMsg
noteListPanel model =
    column [ spacing 20, height (pxFloat config.panelHeight), width (pxFloat <| 0.9 * config.panelWidth - 50), Border.width 1 ]
        [ viewNotes model
        ]


noteNameButton : Maybe Note -> Note -> Element FrontendMsg
noteNameButton maybeCurrentNote note =
    Input.button (Style.titleButton (maybeCurrentNote == Just note))
        { onPress = Just FENoop
        , label = Element.text note.subject
        }


setCurrentNoteButton : Model -> Note -> Int -> Element FrontendMsg
setCurrentNoteButton model note index =
    Input.button (Style.titleButton (Just note == model.maybeCurrentNote))
        { onPress = Just FENoop
        , label = el [ Font.bold ] (Element.text <| String.slice 0 4 note.id)
        }


viewNotes : Model -> Element FrontendMsg
viewNotes model =
    column [ spacing 12, padding 20, height (px 510) ]
        [ el [ Font.size 16, Font.bold ] (text "Notes")
        , indexedTable [ spacing 4, Font.size 12, height (pxFloat (config.panelHeight - 50)), scrollbarY ]
            { data = List.filter (\note -> note.selected) model.notes
            , columns =
                [ { header = el [ Font.bold ] (text <| idLabel model)
                  , width = px 36
                  , view = \k note -> el [ Font.size 12, Font.family [ Font.typeface "Courier New", Font.monospace ] ] (text <| String.slice 0 4 note.id)
                  }
                , { header = el [ Font.bold ] (text <| "Modified")
                  , width = px 90
                  , view = \k note -> el [ Font.size 12 ] (text <| DateTime.humanDateStringFromPosix note.timeModified)
                  }
                , { header = el [ Font.bold ] (text "Subject")
                  , width = px 220
                  , view = \k note -> selectNoteButton model.maybeCurrentNote note
                  }
                ]
            }
        ]


viewNote : Maybe Note -> Element FrontendMsg
viewNote maybeNote =
    case maybeNote of
        Nothing ->
            column [ padding 20, spacing 20, height (pxFloat config.panelHeight), width (pxFloat config.panelWidth), Border.width 1 ]
                [ el [ Font.size 12 ] (text "No note selected")
                ]

        Just note ->
            let
                created =
                    "*Created: " ++ DateTime.humanDateStringFromPosix note.timeCreated ++ "*, "

                modified =
                    "*modified: " ++ DateTime.humanDateStringFromPosix note.timeModified ++ "*\n\n"

                title =
                    "# " ++ note.subject ++ "\n\n"

                words =
                    "**Words:** " ++ String.fromInt (Note.wordCount note) ++ " (" ++ String.fromInt (String.length note.body) ++ " characters)" ++ "\n\n"

                tags =
                    case note.tags of
                        [] ->
                            "Tags: none\n\n"

                        _ ->
                            "**Tags:** " ++ String.join ", " note.tags ++ "\n\n"

                hr =
                    """<hr style="height:1px; border:none; background-color: #333;" />

"""

                content =
                    title ++ created ++ modified ++ words ++ tags ++ hr ++ note.body
            in
            column [ padding 20, spacing 12, height (pxFloat config.panelHeight), width (pxFloat config.panelWidth), Border.width 1 ]
                [ toMarkdown content |> Element.html
                ]

--
-- tagButtons : Model -> Element FrontendMsg
-- tagButtons model =
--     model.frequencyDict
--         |> FrequencyDict.list
--         |> List.map (\item -> tagButton item)
--         |> List.take 20
--         |> (\x -> el [ Font.size 12 ] (text "Tags:") :: x)
--         |> (\x -> x ++ [ clearTagSearch ])
--         |> (\x -> Element.paragraph [ spacing 8, Font.size 14, width (pxFloat (2 * config.panelWidth - 60)) ] x)




clearTagSearch : Element FrontendMsg
clearTagSearch =
    Input.button Style.smallButton
        { onPress = Just ClearAllSearches
        , label = text "Clear tag search"
        }


clearAllSearches : Element FrontendMsg
clearAllSearches =
    Input.button Style.smallButton
        { onPress = Just ClearAllSearches
        , label = text "Clear searches"
        }


selectNoteButton : Maybe Note -> Note -> Element FrontendMsg
selectNoteButton maybeCurrentNote note =
    Input.button (Style.titleButton (maybeCurrentNote == Just note))
        { onPress = Just (SetCurrentNote note)
        , label = text (note.subject |> truncateStringNicely 20)
        }

truncateStringNicely : Int -> String -> String
truncateStringNicely k str =
  case String.length str <= k of
    True -> str
    False -> String.left k str ++ "..."

idLabel : Model -> String
idLabel model =
    "id"



--
-- VIEW NOTE
--


submitNoteButton : Element FrontendMsg
submitNoteButton =
    Input.button Style.button
        { onPress = Just MakeNewNote
        , label = Element.text "New: minutes or hh:mm"
        }



-- STUFF
--
-- FILTERS
--


filterPanel model =
    row [ spacing 12 ]
        [ el [ Font.bold ] (text "Filter by")
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "subject:")
            , inputNoteNameFilter model
            ]
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "text:")
            , inputTextFilter model
            ]
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "tag:")
            , inputTagFilter model
            ]
        , clearAllSearches
        ]



-- , row [ spacing 8 ]
--     [ el [ Font.bold, Font.size 14 ] (text "After")
--     , displayShiftedDate model.noteCameAfterString model.currentTime
--     ]
-- , inputNoteCameAfterFilter model
-- , row [ spacing 8 ]
--     [ el [ Font.bold, Font.size 14 ] (text "Before")
--     , displayShiftedDate model.noteCameBeforeString model.currentTime
--     ]
-- , inputNoteCameBeforeFilter model
--, row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, noteModeButton model ]


displayShiftedDate : String -> Posix -> Element FrontendMsg
displayShiftedDate kDaysAgoString today =
    case String.toInt kDaysAgoString of
        Nothing ->
            el [ width (px 75), Font.bold, Font.size 14 ] (text "(days)")

        Just k ->
            let
                shiftedDate =
                    Note.kDaysAgo k today
            in
            el [ width (px 75), Font.bold, Font.size 14 ] (text <| DateTime.humanDateStringFromPosix shiftedDate)


inputNoteNameFilter model =
    Input.text (Style.inputStyle 160)
        { onChange = GotNoteFilter
        , text = model.noteFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputTextFilter model =
    Input.text (Style.inputStyle 160)
        { onChange = GotTextFilter
        , text = model.textFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputTagFilter model =
    Input.text (Style.inputStyle 80)
        { onChange = GotTagFilter
        , text = model.tagFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteCameBeforeFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotNoteDateBeforeFilter
        , text = model.noteCameBeforeString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputNoteCameAfterFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotNoteDateAfterFilter
        , text = model.noteCameAfterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



-- MANUAL


viewText : String -> Element FrontendMsg
viewText str =
    column
        [ width (pxFloat config.panelWidth)
        , height (pxFloat config.panelHeight)
        , padding 20
        , scrollbarY
        , Border.width 1
        ]
        [ toMarkdown str |> Element.html ]


manual : Element FrontendMsg
manual =
    viewText Text.manual

tagsView : Model -> Element FrontendMsg
tagsView model =
  column
      [ width (pxFloat config.panelWidth)
      , height (pxFloat config.panelHeight)
      , padding 20
      , spacing 6
      , Font.size 14
      , scrollbarY
      , Border.width 1
      ]
      ((tagHeader model)::(tagButtonList model))

tagHeader model =
  row [spacing 12, paddingEach {nullPadding | bottom = 8}] [
    el [Font.bold] (text ("Tags: " ++ String.fromInt (tagCount model)))
    -- , clearTagSearch
    ]

nullPadding = {left = 0, right = 0, top = 0, bottom = 0}

tagCount : Model -> Int
tagCount model =
  model.frequencyDict
      |> FrequencyDict.list
      |> List.length


tagButtonList : Model -> List (Element FrontendMsg)
tagButtonList model =
    model.frequencyDict
        |> FrequencyDict.list
        |> List.map (\item -> tagButton model.selectedTag item )

tagButton : String -> ( String, Int ) ->  Element FrontendMsg
tagButton selectedTag ( tag, freq ) =
    let
        _ = Debug.log "(tag, selectedTag)" (tag, selectedTag)
    in
    Input.button (Style.titleButton ( tag ==  selectedTag) ++ [ paddingXY 4 0 ])
        { onPress = Just (SetTagForSearch tag)
        , label = text (tag ++ ": " ++ String.fromInt freq)
        }


-- MARKDOWN
--


myOptions : Markdown.Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Just "elm"
    , sanitize = False
    , smartypants = False
    }


markdownStyle =
    [ HA.style "font-size" "14px"
    , HA.style "width" (pxString <| config.panelWidth - 40)
    , HA.style "overflow-y" "scroll"
    , HA.style "white-space" "normal"
    , HA.style "line-height" "1.4"
    , HA.class "hr-thin"
    , HA.class "image"

    -- , HA.style "p" "display: inline-block"
    ]



-- Utility


pxString : Float -> String
pxString f =
    String.fromFloat f ++ "px"


toMarkdown : String -> Html FrontendMsg
toMarkdown userInput =
    Markdown.toHtmlWith myOptions markdownStyle userInput


pxFloat =
    round >> px
