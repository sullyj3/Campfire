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

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
    }

-- MODEL -------------------------------------------------------------------

type alias StoryID = Int

type alias Story = { storyMeta : StoryMeta
                   , storyText : String }

type alias StoryMeta = 
  { storyID   : StoryID
  , storyTitle : String }

type ModelState
  = LoadingIndex
  | LoadingStory StoryID
  | SuccessStory Story
  | SuccessIndex (List StoryMeta)
  | StoryNotFound StoryID
  | CouldntConnect -- TODO retry

type alias Model = { apiURL : String
                   , state      : ModelState
                   }


testMd = """
## Header 2 ##

### Here's a list: ###

1. List item 1

   continued

2. List item 2
"""

type alias Flags = { api_url : String }


init : Flags -> (Model, Cmd Msg)
init flags = ( { apiURL = flags.api_url
               , state  = LoadingIndex
               }
             , getIndex flags.api_url
             )

-- UPDATE -------------------------------------------------------------------

type Msg = LoadStory StoryID
         | LoadIndex
         | GotIndex (List StoryMeta)
         | GotStory Story
         | CouldntConnectMsg

getStory : String -> StoryID -> Cmd Msg
getStory apiURL id = Http.get 
  { url = apiURL ++ "/story/" ++ fromInt id
  , expect = Http.expectJson handleStoryHttpResult decodeStory
  }

getIndex : String -> Cmd Msg
getIndex apiURL = Http.get
  { url = apiURL ++ "/stories"
  , expect = Http.expectJson handleHttpGetIndex decodeIndex
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  LoadStory id      -> ({ model | state = LoadingStory id      }, getStory model.apiURL id )
  LoadIndex         -> ({ model | state = LoadingIndex         }, getIndex model.apiURL    )
  GotStory s        -> ({ model | state = SuccessStory s       }, Cmd.none                 )
  GotIndex entries  -> ({ model | state = SuccessIndex entries }, Cmd.none                 )
  CouldntConnectMsg -> ({ model | state = CouldntConnect       }, Cmd.none                 )

decodeIndex : Decoder (List StoryMeta)
decodeIndex = list decodeStoryMeta

decodeStoryMeta : Decoder StoryMeta
decodeStoryMeta = map2 StoryMeta
  (field "storyID" int)
  (field "storyTitle" string)

decodeStory : Decoder Story
decodeStory = map2 Story
  (field "storyMeta" decodeStoryMeta)
  (field "storyText" string)

handleHttpGetIndex : Result Http.Error (List StoryMeta) -> Msg
handleHttpGetIndex r = case r of
  Ok entries -> GotIndex entries
  Err e -> CouldntConnectMsg

handleStoryHttpResult : Result Http.Error Story -> Msg
handleStoryHttpResult r = case r of
  Ok s  -> GotStory s
  Err e -> CouldntConnectMsg

-- VIEW -------------------------------------------------------------------

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
    , button [onClick LoadIndex] [text "Stories"]
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

storyView : Story -> List (Html Msg)
storyView { storyMeta, storyText } = let {storyID, storyTitle} =  storyMeta in
    [ headerBar storyTitle
    , mdView storyText ]

storyMetaLinkView : StoryMeta -> Html Msg
storyMetaLinkView sm =
  button [onClick <| LoadStory sm.storyID] [storyMetaView sm]

storyMetaView : StoryMeta -> Html Msg
storyMetaView {storyID, storyTitle} =
  text <| fromInt storyID ++ ": " ++ storyTitle

indexView : List StoryMeta -> List (Html Msg)
indexView entries =
  [ h1 [] [text "Stories"]
  , ul [] (map (li [] << singleton << storyMetaLinkView) entries) ]

bodyView : Model -> Html Msg
bodyView model =
    div [class "container"]
      <| case model.state of
           LoadingIndex         -> [ text "loading index" ]
           LoadingStory id      -> [ text <| "loading story id " ++ fromInt id ]
           SuccessIndex entries -> indexView entries
           SuccessStory stry    -> storyView stry
           StoryNotFound _      -> [ text "couldn't find the story!" ]
           CouldntConnect       -> [ text "couldn't reach the backend" ]
