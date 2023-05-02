module Icons exposing (..)

import Svg
import VirtualDom exposing (Attribute, attribute)


chevronDown : List (Attribute msg) -> Svg.Svg msg
chevronDown attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "class" "h-6 w-6", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke" "currentColor", attribute "stroke-width" "2"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M19 9l-7 7-7-7"]) []]

chevronUp : List (Attribute msg) -> Svg.Svg msg
chevronUp attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "class" "h-6 w-6", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke" "currentColor", attribute "stroke-width" "2"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M5 15l7-7 7 7"]) []]

clipboardCheck : List (Attribute msg) -> Svg.Svg msg
clipboardCheck attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "class" "h-6 w-6", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke" "currentColor", attribute "stroke-width" "2"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"]) []]

clipboard : List (Attribute msg) -> Svg.Svg msg
clipboard attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "class" "h-6 w-6", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke" "currentColor", attribute "stroke-width" "2"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"]) []]

download : List (Attribute msg) -> Svg.Svg msg
download attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "class" "h-6 w-6", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke" "currentColor", attribute "stroke-width" "2"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"]) []]