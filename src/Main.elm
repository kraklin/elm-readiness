module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Set exposing (Set)



---- DEMO DATA ----


demoData : String
demoData =
    """{
    "version": "0.0.1",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "BSD3",
    "source-directories": [
        "src"
    ],
    "exposed-modules": [],
    "dependencies": {
        "NoRedInk/elm-compare": "1.1.0 <= v < 2.0.0",
        "cuducos/elm-format-number": "5.0.3 <= v < 6.0.0",
        "elm-community/json-extra": "2.1.0 <= v < 3.0.0",
        "elm-community/list-extra": "5.0.0 <= v < 6.0.0",
        "elm-community/maybe-extra": "4.0.0 <= v < 5.0.0",
        "elm-community/parser-combinators": "1.0.0 <= v < 2.0.0",
        "elm-lang/core": "5.0.0 <= v < 6.0.0",
        "elm-lang/dom": "1.1.1 <= v < 2.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0",
        "elm-lang/http": "1.0.0 <= v < 2.0.0",
        "elm-lang/keyboard": "1.0.1 <= v < 2.0.0",
        "elm-lang/mouse": "1.0.1 <= v < 2.0.0",
        "elm-lang/navigation": "2.0.1 <= v < 3.0.0",
        "elm-lang/window": "1.0.1 <= v < 2.0.0",
        "eskimoblood/elm-color-extra": "5.1.0 <= v < 6.0.0",
        "etaque/elm-form": "3.0.0 <= v < 4.0.0",
        "evancz/elm-markdown": "3.0.2 <= v < 4.0.0",
        "evancz/url-parser": "2.0.1 <= v < 3.0.0",
        "janjelinek/creditcard-validation": "1.0.0 <= v < 2.0.0",
        "krisajenkins/remotedata": "4.3.3 <= v < 5.0.0",
        "mdgriffith/elm-color-mixing": "1.1.1 <= v < 2.0.0",
        "mgold/elm-date-format": "1.6.0 <= v < 2.0.0",
        "mpizenberg/elm-debounce": "3.0.2 <= v < 4.0.0",
        "nonpop/elm-purl": "2.1.0 <= v < 3.0.0",
        "stoeffel/set-extra": "1.2.2 <= v < 2.0.0",
        "wernerdegroot/listzipper": "3.0.0 <= v < 4.0.0",
        "zaboco/elm-draggable": "2.0.2 <= v < 2.1.0"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
"""



---- MODEL ----


type alias Model =
    { availablePackages : Result Json.Decode.Error (List String)
    , myElmPackageTextArea : String
    , result : RemoteData ReadinessError ReadinessResult
    }


type DependencyStatus
    = Ready
    | ReplacedWith String
    | NotReady


type ReadinessError
    = JsonParsingError Json.Decode.Error
    | Other String


type alias ReadinessResult =
    Dict String DependencyStatus


init : String -> ( Model, Cmd Msg )
init searchJson =
    let
        packages =
            Json.Decode.decodeString packagesDecoder searchJson
    in
    ( { availablePackages = packages
      , myElmPackageTextArea = ""
      , result = NotAsked
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CheckPackages
    | UpdateTextArea String
    | LoadDemoData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTextArea content ->
            ( { model | myElmPackageTextArea = content }, Cmd.none )

        LoadDemoData ->
            ( { model | myElmPackageTextArea = demoData }, Cmd.none )

        CheckPackages ->
            let
                dependencies =
                    Json.Decode.decodeString dependenciesDecoder model.myElmPackageTextArea
                        |> Result.map (List.map Tuple.first)

                result =
                    case ( dependencies, model.availablePackages ) of
                        ( Ok dep, Ok packages ) ->
                            Success <| getResult dep packages

                        ( Err err, _ ) ->
                            Failure <| JsonParsingError err

                        ( _, _ ) ->
                            Failure <| Other "Something went wrong"
            in
            ( { model | result = result }, Cmd.none )


packagesDecoder =
    Json.Decode.list <| Json.Decode.field "name" Json.Decode.string


dependenciesDecoder =
    Json.Decode.field "dependencies" (Json.Decode.keyValuePairs Json.Decode.string)


replacedPackages : Dict String String
replacedPackages =
    Dict.fromList
        [ ( "NoRedInk/elm-decode-pipeline", "NoRedInk/elm-json-decode-pipeline" )
        , ( "elm-community/elm-test", "elm-explorations/test" )
        , ( "elm-lang/animation-frame", "elm/browser" )
        , ( "elm-lang/core", "elm/core" )
        , ( "elm-lang/html", "elm/html" )
        , ( "elm-lang/http", "elm/http" )
        , ( "elm-lang/svg", "elm/svg" )
        , ( "elm-lang/virtual-dom", "elm/virtual-dom" )
        , ( "elm-tools/parser", "elm/parser" )
        , ( "evancz/elm-markdown", "elm-explorations/markdown" )
        , ( "evancz/url-parser", "elm/url" )
        , ( "mgold/elm-random-pcg", "elm/random" )
        , ( "ohanhi/keyboard-extra", "ohanhi/keyboard" )
        , ( "thebritican/elm-autocomplete", "ContaSystemer/elm-menu" )
        , ( "elm-community/linear-algebra", "elm-explorations/linear-algebra" )
        , ( "elm-community/webgl", "elm-explorations/webgl" )
        , ( "elm-lang/keyboard", "elm/browser" )
        , ( "elm-lang/dom", "elm/browser" )
        , ( "elm-lang/navigation", "elm/browser" )
        , ( "elm-lang/window", "elm/browser" )
        , ( "mpizenberg/elm-mouse-events", "mpizenberg/elm-pointer-events" )
        , ( "mpizenberg/elm-touch-events", "mpizenberg/elm-pointer-events" )
        , ( "ryannhg/elm-date-format", "ryannhg/date-format" )
        , ( "rtfeldman/hex", "rtfeldman/elm-hex" )
        , ( "elm-lang/mouse", "elm/browser" )
        , ( "avh4/elm-transducers", "avh4-experimental/elm-transducers" )
        , ( "dillonkearns/graphqelm", "dillonkearns/elm-graphql" )
        ]


getResult : List String -> List String -> ReadinessResult
getResult myDependencies packages =
    let
        dependencyStatus dependency =
            if List.member dependency packages then
                Ready

            else
                Dict.get dependency replacedPackages
                    |> Maybe.map (\replaced -> ReplacedWith replaced)
                    |> Maybe.withDefault NotReady
    in
    List.map (\dep -> ( dep, dependencyStatus dep )) myDependencies
        |> Dict.fromList



---- VIEW ----


viewDependency : ( String, DependencyStatus ) -> Element Msg
viewDependency ( name, status ) =
    let
        readyColor =
            Element.rgb 0 1 0

        notReadyColor =
            Element.rgb 1 0 0

        statusColor =
            case status of
                NotReady ->
                    notReadyColor

                _ ->
                    readyColor

        statusMarker =
            Element.el [ Background.color statusColor, Element.width <| Element.px 5, Element.height Element.fill ] <| Element.text " "

        links packageName =
            Element.row [ Element.spacing 10, Element.paddingEach { top = 10, left = 0, right = 0, bottom = 0 } ]
                [ Element.newTabLink []
                    { url = "https://github.com/" ++ packageName
                    , label =
                        Element.image
                            [ Element.width <| Element.px 16 ]
                            { src = "github-icon.png", description = "GitHub" }
                    }
                , Element.newTabLink []
                    { url = "https://package.elm-lang.org/packages/" ++ packageName
                    , label =
                        Element.image [ Element.width <| Element.px 16 ]
                            { src = "elm-lang-icon.png", description = "Elm Package" }
                    }
                ]

        dependency =
            case status of
                ReplacedWith replacedName ->
                    [ Element.el [ Font.size 10, Font.color <| Element.rgb 0.5 0.5 0.5 ] <| Element.text <| "(" ++ name ++ ")"
                    , Element.el [ Font.size 14, Font.bold ] <| Element.text replacedName
                    , links replacedName
                    ]

                _ ->
                    [ Element.el [ Font.size 14, Font.bold ] <| Element.text name
                    , links name
                    ]
    in
    Element.wrappedRow
        [ Border.width 1
        , Element.width <| Element.px 300
        ]
        [ statusMarker
        , Element.column [ Element.padding 10 ] dependency
        ]


viewResult : RemoteData ReadinessError ReadinessResult -> List (Element Msg)
viewResult readinessResult =
    let
        partitionResult result =
            Dict.partition (\k v -> v /= NotReady) result

        dictView header result =
            Element.column
                [ Font.size 12
                , Element.spacing 20
                , Element.alignTop
                , Element.padding 10
                ]
                [ Element.text <| header ++ ": " ++ (String.fromInt <| Dict.size result)
                , Element.column [ Element.spacing 5 ] <|
                    (Dict.toList result
                        |> List.map viewDependency
                    )
                ]

        viewBoth result =
            partitionResult result
                |> (\( ready, notReady ) -> [ dictView "Packages that are not ready yet" notReady, dictView "Packages already on Elm 0.19" ready ])
    in
    case readinessResult of
        Success result ->
            viewBoth result

        Failure error ->
            case error of
                JsonParsingError err ->
                    [ Element.text <| "Unable to parse pasted elm-package.json. Take a look at it." ]

                Other err ->
                    [ Element.text <| "Something went wrong:" ++ err ]

        _ ->
            [ Element.none ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.padding 10, Element.spacing 10 ]
            [ Element.el [] <| Element.text "Elm 0.19 readiness checker"
            , Element.wrappedRow [ Element.spacing 40 ] <|
                Element.column [ Element.alignTop, Element.spacing 20 ]
                    [ Input.button
                        [ Element.centerX
                        , Font.size 11
                        , Border.width 1
                        , Element.padding 3
                        , Border.rounded 4
                        ]
                        { onPress = Just LoadDemoData
                        , label = Element.text "Load demo elm-package.json"
                        }
                    , Input.multiline
                        [ Element.width <| Element.px 400
                        , Element.height <| Element.px 500
                        , Font.size 11
                        ]
                        { onChange = UpdateTextArea
                        , text = model.myElmPackageTextArea
                        , label = Input.labelAbove [ Font.size 14 ] <| Element.text "Paste content of your elm-package.json here"
                        , placeholder = Nothing
                        , spellcheck = False
                        }
                    , Input.button
                        [ Element.centerX
                        , Border.width 1
                        , Element.padding 10
                        , Border.rounded 4
                        ]
                        { onPress = Just CheckPackages
                        , label = Element.text "Check my packages"
                        }
                    ]
                    :: viewResult model.result
            ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
