module Style exposing
    ( activeButton
    , activeButtonDarkRed
    , activeButtonRed
    , black
    , blue
    , button
    , buttonWithWidth
    , charcoal
    , dangerousButton
    , darkBlue
    , darkRed
    , footer
    , footerForPhone
    , footerItem
    , grey
    , headerButton
    , inactiveButton
    , inputStyle
    , lightGrey
    , listElementButtonStyleWithWidth2
    , listItemButton
    , mainColumn
    , mainColumn2
    , mainColumnPhone
    , mainColumnX
    , makeGrey
    , multiline
    , myFocusStyle
    , navBar
    , navBarPhone
    , noAutocapitalize
    , noAutocorrect
    , orange
    , preWrap
    , red
    , select
    , selectedHeaderButton
    , shadedColumn
    , signinColumn
    , smallButton
    , smallButtonRed
    , tableHeading
    , titleButton
    , titleButton2
    , white
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


select : Bool -> List (Element.Attr () msg) -> List (Element.Attr () msg) -> List (Element.Attr () msg)
select bit style1 style2 =
    case bit of
        True ->
            style1

        False ->
            style2


tableHeading =
    [ Font.bold ]


titleButton highlighted =
    if highlighted then
        [ Font.color darkRed, Font.bold ]

    else
        [ Font.color darkBlue ]


titleButton2 highlighted =
    if highlighted then
        [ Font.color orange ]

    else
        [ Font.color lightOrange ]


buttonWithWidth width_ =
    [ Font.size 13
    , width (px width_)
    , Background.color black
    , Font.color (makeGrey 0.9)
    , Element.paddingXY 10 6
    ]
        ++ basicButtonsStyle


button : List (Element.Attr () msg)
button =
    [ Background.color black, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


listItemButton : List (Element.Attr () msg)
listItemButton =
    basicButtonsStyle


dangerousButton : List (Element.Attr () msg)
dangerousButton =
    [ Background.color darkRed, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


headerButton : List (Element.Attr () msg)
headerButton =
    [ Background.color white, Font.color black, Element.paddingXY 10 6 ] ++ basicButtonsStyle


selectedHeaderButton : List (Element.Attr () msg)
selectedHeaderButton =
    [ Background.color black, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


inactiveButton : List (Element.Attr () msg)
inactiveButton =
    [ Background.color (makeGrey 0.5)
    , Font.color white
    , Element.paddingXY 10 6
    , buttonFontSize
    , pointer
    ]


activeButton : Bool -> List (Element.Attr () msg)
activeButton active =
    case active of
        True ->
            activeButtonStyle

        False ->
            button


activeButtonRed : Bool -> List (Element.Attr () msg)
activeButtonRed active =
    case active of
        True ->
            activeButtonRedStyle

        False ->
            button


activeButtonDarkRed : Bool -> List (Element.Attr () msg)
activeButtonDarkRed active =
    case active of
        True ->
            activeButtonDarkRedStyle

        False ->
            button


smallButton =
    basicButtonsStyle ++ [ Background.color charcoal, Font.color grey, Font.size 12, Element.paddingXY 6 4 ]


smallButtonRed =
    [ Background.color (Element.rgb 0.9 0.0 0.0), Font.color grey, Font.size 12, Element.paddingXY 6 4, alignRight ] ++ basicButtonsStyle


mainColumn w h =
    [ paddingXY 8 8, spacing 12, width w, height h, clipY, clipX ]


mainColumnX =
    mainColumn fill fill ++ [ spacing 12, padding 12, Background.color (makeGrey 0.9) ]


mainColumn2 w h =
    [ paddingXY 8 8, spacing 12, width w, height h, Background.color grey, clipY, clipX ]


mainColumnPhone w h =
    [ spacing 12, width w, height h, Background.color grey, clipY, clipX ]


shadedColumn w h =
    [ paddingXY 24 24, spacing 24, Background.color lightBlue, width w, height h ]


signinColumn w h =
    [ paddingXY 24 24, spacing 24, Background.color (rgb255 252 240 209), width w, height h ]


navBar w =
    [ spacing 24, Background.color charcoal, paddingXY 12 8, width w ]


navBarPhone w =
    [ spacing 8, Background.color charcoal, paddingXY 12 8, width w ]


footer =
    [ spacing 24, Background.color charcoal, paddingXY 12 8, alignBottom, width fill, Font.size 14 ]


footerForPhone =
    [ spacing 8, Background.color charcoal, paddingXY 12 8, alignBottom, width fill, Font.size 12 ]


footerItem =
    [ Font.color white ]


noAutocapitalize =
    Element.htmlAttribute (Html.Attributes.attribute "autocapitalize" "none")


noAutocorrect =
    Element.htmlAttribute (Html.Attributes.attribute "autocorrect" "off")


preWrap =
    Element.htmlAttribute (Html.Attributes.attribute "white-space" "pre-wrap")



--
-- PARAMETERS
--


buttonFontSize =
    Font.size 16


myFocusStyle =
    focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing }



--
-- HELPERS
--


basicButtonsStyle =
    [ buttonFontSize
    , pointer
    , mouseDown [ buttonFontSize, Background.color mouseDownColor ]
    ]


activeButtonStyle : List (Element.Attr () msg)
activeButtonStyle =
    [ Background.color darkBlue, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


activeButtonRedStyle : List (Element.Attr () msg)
activeButtonRedStyle =
    [ Background.color (Element.rgb 1 0 0), Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


activeButtonDarkRedStyle : List (Element.Attr () msg)
activeButtonDarkRedStyle =
    [ Background.color darkRed, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle



--
-- SHARED BOOKS
--


listElementButtonStyleWithWidth2 width_ selected_ =
    -- if selected_ == False then
    [ width (px width_)
    , Background.color charcoal
    , Font.color white
    , Font.size 12
    , Element.paddingXY 8 4
    , alignRight
    ]
        ++ basicButtonsStyle



--
--    else
--        [ width (px width_),  Font.color white, Font.size 12, Element.paddingXY 6 4, alignRight ]
--            ++ basicButtonsStyle
--
-- COLORS
--


grey =
    makeGrey 0.95


charcoal =
    Element.rgb 0.4 0.4 0.4


makeGrey g =
    Element.rgb g g g


lightGrey =
    makeGrey 0.95


darkRed =
    Element.rgb 0.5 0.0 0.0


red =
    Element.rgb 1.0 0.0 0.0


pink =
    Element.rgb 1.0 0.9 0.9


white =
    Element.rgb 1 1 1


black =
    Element.rgb 0.1 0.1 0.1


mouseOverColor =
    Element.rgb 0.0 0.6 0.9


mouseDownColor =
    Element.rgb 0.7 0.1 0.1


lightBlue =
    Element.rgb 0.8 0.8 0.9


mediumBlue =
    Element.rgb 0.7 0.7 1.0


blue =
    Element.rgb 0.15 0.15 1.0


darkBlue =
    Element.rgb 0.0 0.0 0.6


orange =
    Element.rgb 1.0 0.7 0.0549


darkOrange =
    Element.rgb 0.8 0.5 0.0549


lightOrange =
    Element.rgb255 255 239 204


inputStyle w =
    [ width (px w)
    , height (px 30)
    , Background.color (makeGrey 0.8)
    , Font.color black
    , Font.size 12
    , Border.width 2
    ]


multiline w h =
    [ width (px w)
    , height (px h)
    , Background.color (makeGrey 0.8)
    , Font.color black
    , Font.size 12
    , Border.width 2
    , scrollbarY
    ]
