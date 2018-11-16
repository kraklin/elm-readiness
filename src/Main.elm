module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Navigation
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
import Task
import Url exposing (Url)
import Url.Builder as Url



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
        "justinmimbs/elm-date-extra": "3.0.0 <= v < 4.0.0",
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
    { availablePackages : WebData (List String)
    , elmPackageInput : String
    , result : RemoteData ReadinessError ReadinessResult
    , currentPage : Page
    , navigationKey : Navigation.Key
    }


type DependencyStatus
    = Ready
    | ReplacedWith (List String)
    | NotReady


type ReadinessError
    = JsonParsingError Json.Decode.Error
    | SearchJsonFailure String
    | GitHubRepoError String
    | Other String


type alias ReadinessResult =
    Dict String DependencyStatus


type Page
    = HomePage
    | PackagePage String


urlToPage : Url -> Page
urlToPage url =
    url.fragment
        |> Maybe.map
            (\f ->
                if String.isEmpty f then
                    HomePage

                else
                    PackagePage f
            )
        |> Maybe.withDefault HomePage


packageRequest : String -> Cmd Msg
packageRequest package =
    let
        requestUrl =
            "https://raw.githubusercontent.com/" ++ package ++ "/master/elm-package.json"
    in
    Http.send (GetPackageFromGitHub package) <| Http.getString requestUrl


searchJsonRequest : Cmd Msg
searchJsonRequest =
    RemoteData.Http.get "https://cors-anywhere.herokuapp.com/https://package.elm-lang.org/search.json" HandleElmPackages packagesDecoder


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { availablePackages = Loading
      , elmPackageInput = ""
      , result = NotAsked
      , currentPage = urlToPage url
      , navigationKey = key
      }
    , searchJsonRequest
    )



---- UPDATE ----


type Msg
    = CheckPackages
    | HandleElmPackages (WebData (List String))
    | UpdateTextArea String
    | LoadDemoData
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | GetPackageFromGitHub String (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTextArea content ->
            ( { model | elmPackageInput = content }, Cmd.none )

        LoadDemoData ->
            ( { model | elmPackageInput = demoData }, Cmd.none )

        HandleElmPackages data ->
            let
                nextCommand =
                    case model.currentPage of
                        PackagePage package ->
                            packageRequest package

                        HomePage ->
                            Cmd.none
            in
            ( { model | availablePackages = data }, nextCommand )

        CheckPackages ->
            let
                dependencies =
                    Json.Decode.decodeString dependenciesDecoder model.elmPackageInput
                        |> Result.map (List.map Tuple.first)

                result =
                    case ( dependencies, model.availablePackages ) of
                        ( Ok dep, Success packages ) ->
                            Success <| getResult dep packages

                        ( Err err, _ ) ->
                            Failure <| JsonParsingError err

                        ( _, _ ) ->
                            Failure <| Other "Something went wrong"
            in
            if String.isEmpty model.elmPackageInput then
                ( model, Cmd.none )

            else
                ( { model | result = result }, Cmd.none )

        GetPackageFromGitHub repo package ->
            let
                dependencies =
                    case package of
                        Ok packageContent ->
                            Json.Decode.decodeString dependenciesDecoder packageContent
                                |> Result.map (List.map Tuple.first)
                                |> Result.mapError (\err -> JsonParsingError err)

                        Err httpErr ->
                            Err <| GitHubRepoError repo

                result =
                    case ( dependencies, model.availablePackages ) of
                        ( Ok dep, Success packages ) ->
                            Success <| getResult dep packages

                        ( _, Failure reason ) ->
                            Failure <| SearchJsonFailure "Probelm with getting search.json"

                        ( Err err, _ ) ->
                            Failure err

                        ( _, _ ) ->
                            Failure <| Other "Something went wrong"
            in
            ( { model | result = result }, Cmd.none )

        UrlRequested request ->
            case request of
                Browser.External url ->
                    ( model, Navigation.load url )

                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navigationKey <| Url.toString url )

        UrlChanged url ->
            let
                nonEmptyFragment =
                    Maybe.andThen
                        (\frag ->
                            if String.isEmpty frag then
                                Nothing

                            else
                                Just frag
                        )
                        url.fragment
            in
            case nonEmptyFragment of
                Just fragment ->
                    ( { model | currentPage = urlToPage url, result = NotAsked }
                    , packageRequest fragment
                    )

                Nothing ->
                    ( { model | currentPage = urlToPage url, result = NotAsked }
                    , Task.perform identity <| Task.succeed CheckPackages
                    )


packagesDecoder : Json.Decode.Decoder (List String)
packagesDecoder =
    Json.Decode.list <| Json.Decode.field "name" Json.Decode.string


dependenciesDecoder : Json.Decode.Decoder (List ( String, String ))
dependenciesDecoder =
    Json.Decode.field "dependencies" (Json.Decode.keyValuePairs Json.Decode.string)


replacedPackages : Dict String (List String)
replacedPackages =
    Dict.fromList
        [ ( "NoRedInk/elm-decode-pipeline", [ "NoRedInk/elm-json-decode-pipeline" ] )
        , ( "elm-community/elm-test", [ "elm-explorations/test" ] )
        , ( "elm-lang/animation-frame", [ "elm/browser" ] )
        , ( "elm-lang/core", [ "elm/core" ] )
        , ( "elm-lang/html", [ "elm/html" ] )
        , ( "elm-lang/http", [ "elm/http" ] )
        , ( "elm-lang/svg", [ "elm/svg" ] )
        , ( "elm-lang/virtual-dom", [ "elm/virtual-dom" ] )
        , ( "elm-tools/parser", [ "elm/parser" ] )
        , ( "evancz/elm-markdown", [ "elm-explorations/markdown" ] )
        , ( "evancz/url-parser", [ "elm/url" ] )
        , ( "mgold/elm-random-pcg", [ "elm/random" ] )
        , ( "ohanhi/keyboard-extra", [ "ohanhi/keyboard" ] )
        , ( "thebritican/elm-autocomplete", [ "ContaSystemer/elm-menu" ] )
        , ( "elm-community/linear-algebra", [ "elm-explorations/linear-algebra" ] )
        , ( "elm-community/webgl", [ "elm-explorations/webgl" ] )
        , ( "elm-lang/keyboard", [ "elm/browser" ] )
        , ( "elm-lang/dom", [ "elm/browser" ] )
        , ( "elm-lang/navigation", [ "elm/browser" ] )
        , ( "elm-lang/window", [ "elm/browser" ] )
        , ( "mpizenberg/elm-mouse-events", [ "mpizenberg/elm-pointer-events" ] )
        , ( "mpizenberg/elm-touch-events", [ "mpizenberg/elm-pointer-events" ] )
        , ( "ryannhg/elm-date-format", [ "ryannhg/date-format" ] )
        , ( "rtfeldman/hex", [ "rtfeldman/elm-hex" ] )
        , ( "elm-lang/mouse", [ "elm/browser" ] )
        , ( "avh4/elm-transducers", [ "avh4-experimental/elm-transducers" ] )
        , ( "dillonkearns/graphqelm", [ "dillonkearns/elm-graphql" ] )
        , ( "mgold/elm-date-format", [ "ryannhg/date-format" ] )
        , ( "justinmimbs/elm-date-extra", [ "justinmimbs/date", "rtfeldman/elm-iso8601-date-strings", "ryannhg/date-format" ] )
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

        dependencyItem itemName =
            Element.column []
                [ Element.el [ Font.size 14, Font.bold ] <| Element.text itemName
                , links itemName
                ]

        dependency =
            case status of
                ReplacedWith replacedNames ->
                    [ Element.el [ Font.size 10, Font.color <| Element.rgb 0.5 0.5 0.5 ] <| Element.text <| "(" ++ name ++ ")"
                    , Element.column [ Element.spacing 5 ]
                        (List.map dependencyItem replacedNames
                            |> List.intersperse (Element.el [] (Element.text "or"))
                        )
                    ]

                Ready ->
                    [ dependencyItem name ]

                NotReady ->
                    [ Element.link [ Font.size 14, Font.bold, Font.underline ] { url = "#" ++ name, label = Element.text name }
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
            if Dict.isEmpty result then
                Element.none

            else
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
            [ Element.paragraph [ Font.size 12, Font.color <| Element.rgb 1 0 0 ] <|
                case error of
                    JsonParsingError err ->
                        [ Element.text <| "Unable to parse pasted elm-package.json. Take a look at it." ]

                    GitHubRepoError repo ->
                        [ Element.text "Problem with downloading stuff from GitHub. Check if the repository "
                        , Element.link [ Font.underline ] { url = "https://github.com/" ++ repo, label = Element.text repo }
                        , Element.text " exists and that it has `elm-package.json` in the root of the repository."
                        ]

                    SearchJsonFailure reason ->
                        [ Element.text <| "Cannot get search.json from packages.elm-lang.org: " ++ reason ]

                    Other err ->
                        [ Element.text <| "Something went wrong: " ++ err ]
            ]

        _ ->
            [ Element.none ]


viewHomepage : Model -> List (Element Msg)
viewHomepage model =
    let
        button =
            case model.availablePackages of
                Success _ ->
                    Input.button
                        [ Element.centerX
                        , Border.width 1
                        , Element.padding 10
                        , Border.rounded 4
                        ]
                        { onPress = Just CheckPackages
                        , label = Element.text "Check my dependencies"
                        }

                Loading ->
                    Element.text "Loading packages..."

                Failure reason ->
                    Element.text "Unable to load packages from packages.elm-lang.org"

                NotAsked ->
                    Element.none
    in
    [ Element.wrappedRow [ Element.spacing 40 ] <|
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
                , Element.height <| Element.px 300
                , Font.size 11
                ]
                { onChange = UpdateTextArea
                , text = model.elmPackageInput
                , label = Input.labelAbove [ Font.size 12 ] <| Element.text "Your elm-package.json"
                , placeholder = Just <| Input.placeholder [ Font.size 14 ] <| Element.text "Paste content of your elm-package.json here"
                , spellcheck = False
                }
            , button
            ]
            :: viewResult model.result
    ]


viewPackagePage : Model -> String -> List (Element Msg)
viewPackagePage model package =
    let
        packages =
            case model.availablePackages of
                Loading ->
                    Element.text "Loading packages..."

                _ ->
                    Element.wrappedRow [] <| viewResult model.result
    in
    [ Element.column [ Element.alignTop, Element.spacing 20 ] <|
        [ Element.link
            [ Font.size 11
            , Border.width 1
            , Element.padding 7
            , Border.rounded 4
            ]
            { url = "#", label = Element.text "< Go back to homepage" }
        , Element.el [ Font.bold, Font.size 18 ] <| Element.text package
        , packages
        ]
    ]


viewGithubLink : Element Msg
viewGithubLink =
    Element.row [ Element.spacing 4, Font.size 14 ]
        [ Element.image
            [ Element.width <| Element.px 16 ]
            { src = "github-icon.png", description = "GitHub" }
        , Element.link [ Font.underline ] { url = "https://github.com/kraklin/elm-readiness/", label = Element.text "Source code" }
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 0.19 Readiness helper"
    , body =
        [ Element.layout [] <|
            Element.column [ Element.padding 10, Element.spacing 10 ] <|
                ([ Element.el [ Font.bold, Font.size 24 ] <| Element.text "Elm 0.19 readiness helper"
                 , viewGithubLink
                 ]
                    ++ (case model.currentPage of
                            HomePage ->
                                viewHomepage model

                            PackagePage package ->
                                viewPackagePage model package
                       )
                )
        ]
    }



---- PROGRAM ----


main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
