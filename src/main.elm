import Browser exposing (Document, document)

import Markdown
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, class, href)
import Http
import Json.Decode exposing (Decoder, at, field, map2, map3, int, string, list)
import Json.Encode as E
import String exposing (fromInt)
import List exposing (map, singleton)

backendURL = "http://localhost:5000"

main =
  -- Browser.sandbox { init = init, update = update, view = view }
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
    }

-- MODEL
type alias Story = { storyid   : StoryID
                   , storyName : String
                   , storyText : String
                   }

type alias StoryID = Int

type alias StoryBlurb = 
  { storyid   : StoryID
  , storyName : String }

type Model
  = Loading
  | LoadingStory StoryID
  | SuccessStory Story
  | SuccessStoryBlurbs (List StoryBlurb)
  | CouldntConnect
  | StoryNotFound StoryID


testMd = """
## Header 2 ##

### Here's a list: ###

1. List item 1

   continued

2. List item 2
"""


init : () -> (Model, Cmd Msg)
init _ = ( Loading
         , getStories
         )


-- UPDATE

type Msg = LoadStory StoryID
         | LoadStories
         | GotStories (List StoryBlurb)
         | GotStory Story
         | CouldntConnectMsg

getStory : StoryID -> Cmd Msg
getStory id = Http.get { url = backendURL ++ "/story/" ++ fromInt id
                       , expect = Http.expectJson handleHttpResult decodeStory 
                       }

getStories : Cmd Msg
getStories = Http.get
             { url = backendURL ++ "/stories"
             , expect = Http.expectJson handleHttpGetStoryBlurbs decodeStoryBlurbs
             }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  LoadStory id -> (LoadingStory id, getStory id)
  LoadStories  -> (Loading, getStories)
  GotStory s -> (SuccessStory s, Cmd.none)
  GotStories sbs -> (SuccessStoryBlurbs sbs, Cmd.none)
  CouldntConnectMsg -> (CouldntConnect, Cmd.none)

decodeStoryBlurbs : Decoder (List StoryBlurb)
decodeStoryBlurbs = at ["data"] <| list decodeStoryBlurb

decodeStoryBlurb : Decoder StoryBlurb
decodeStoryBlurb = map2 StoryBlurb
  (field "storyid" int)
  (field "title" string)

decodeStory : Decoder Story
decodeStory = at ["data"]
  <| map3 Story
       (field "storyid" int)
       (field "title" string)
       (field "content" string)

handleHttpGetStoryBlurbs : Result Http.Error (List StoryBlurb) -> Msg
handleHttpGetStoryBlurbs r = case r of
  Ok sbs -> GotStories sbs
  Err e -> CouldntConnectMsg

handleHttpResult : Result Http.Error Story -> Msg
handleHttpResult r = case r of
  Ok s  -> GotStory s
  Err e -> CouldntConnectMsg

-- VIEW

view : Model -> Document Msg
view model =
  Document "Campfire"
    [ navBar
    , bodyView model
    ]

navBar : Html Msg
navBar =
  nav [class "navbar"]
    [ a [href "upload"] [text "+"]
    , button [onClick LoadStories] [text "Stories"]
    ]

headerBar : String -> Html Msg
headerBar header =
  div [class "row", class "header-bar"]
    [ div [class "col-sm"]
        [ h1 [class "text-center"] [text header] ]
    ]

mdView : String -> Html Msg
mdView md = div [class "mdView", class "row"]
                [ div [class "col-6 offset-3"]
                    <| Markdown.toHtml Nothing md
                ]

storyView : String -> String -> List (Html Msg)
storyView title text =
    [ headerBar title
    , mdView text ]

storyBlurbLinkView : StoryBlurb -> Html Msg
storyBlurbLinkView sb = 
  button [onClick <| LoadStory sb.storyid] [storyBlurbView sb]

storyBlurbView : StoryBlurb -> Html Msg
storyBlurbView {storyid, storyName} =
  text <| fromInt storyid ++ ": " ++ storyName

storyBlurbsView : List StoryBlurb -> List (Html Msg)
storyBlurbsView sbs =
  [ h1 [] [text "Stories"]
  , ul [] (map (li [] << singleton << storyBlurbLinkView) sbs) ]

bodyView : Model -> Html Msg
bodyView model =
    div [class "container"]
      <| case model of
           Loading        -> [ text "loading" ]
           LoadingStory id -> [ text <| "loading story id " ++ fromInt id ]
           SuccessStoryBlurbs sbs -> storyBlurbsView sbs
           SuccessStory {storyid, storyName, storyText} -> storyView storyName storyText
           CouldntConnect   -> [ text "couldn't reach the backend" ]
           StoryNotFound _ -> [ text "couldn't find the story!" ]

