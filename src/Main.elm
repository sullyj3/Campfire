import Browser exposing (Document, document)

import Markdown
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, at, field, map2, map3, int, string, list)
import Json.Encode as Enc
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

type alias StoryUpload =
  { storyTitle : String
  , storyText : String
  }

type ModelState
  = UploadPage StoryUpload
  | LoadingIndex
  | LoadingStory StoryID
  | SuccessStory Story
  | SuccessIndex (List StoryMeta)
  | StoryNotFound StoryID
  | CouldntConnect -- TODO retry

type alias Model = { apiURL : String
                   , state  : ModelState
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
         | GoToUploadPage
         | UploadStory
         | UploadedStory (Result Http.Error ())
         | CouldntConnectMsg
         | UpdateStoryTitle String
         | UpdateStoryText  String


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

uploadStory : String -> StoryUpload -> Cmd Msg
uploadStory apiURL stry = Http.post
  { url = apiURL
  , body = Http.jsonBody <| encodeStoryUpload stry
  , expect = Http.expectWhatever UploadedStory }

emptyStoryUpload : StoryUpload
emptyStoryUpload = StoryUpload "" ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  LoadStory id       -> ({ model | state = LoadingStory id      }, getStory model.apiURL id )
  LoadIndex          -> ({ model | state = LoadingIndex         }, getIndex model.apiURL    )
  GotStory s         -> ({ model | state = SuccessStory s       }, Cmd.none                 )
  GotIndex entries   -> ({ model | state = SuccessIndex entries }, Cmd.none                 )
  GoToUploadPage     -> ({ model | state = UploadPage emptyStoryUpload}, Cmd.none                 )
  CouldntConnectMsg  -> (  model                                 , Cmd.none                 )
  UpdateStoryTitle t -> case model.state of
                          (UploadPage currFormState) ->
                            ({ model | state = UploadPage (updateStoryTitle t currFormState) }, Cmd.none )
                          _ -> (model, Cmd.none)
  UpdateStoryText t  -> case model.state of
                          (UploadPage currFormState) ->
                            ({ model | state = UploadPage (updateStoryText  t currFormState) }, Cmd.none )
                          _ -> (model, Cmd.none)
  UploadStory   -> let 
                        storyUpload = case model.state of
                          UploadPage su -> su
                          _             -> emptyStoryUpload -- unreachable, hopefully
                        cmd = uploadStory model.apiURL storyUpload

                   in
                       (model, cmd)
  UploadedStory _ -> (model, Cmd.none)

update2 : (Model, Msg) -> (Model, Cmd Msg)

updateStoryTitle : String -> StoryUpload -> StoryUpload
updateStoryTitle t su = { su | storyTitle = t }

updateStoryText : String -> StoryUpload -> StoryUpload
updateStoryText t su = { su | storyText = t }

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

encodeStoryUpload : StoryUpload -> Enc.Value
encodeStoryUpload stry = Enc.object
    [ ("storyTitle", Enc.string stry.storyTitle)
    , ("storyText" , Enc.string stry.storyText )
    ]

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

bodyView : Model -> Html Msg
bodyView model =
    div [class "container"]
      <| case model.state of
           UploadPage _          -> uploadView
           LoadingIndex         -> [ text "loading index" ]
           LoadingStory id      -> [ text <| "loading story id " ++ fromInt id ]
           SuccessIndex entries -> indexView entries
           SuccessStory stry    -> storyView stry
           StoryNotFound _      -> [ text "couldn't find the story!" ]
           CouldntConnect       -> [ text "couldn't reach the backend" ]

navBar : Html Msg
navBar =
  nav [class "navbar"]
    [ a [href "upload"] [text "+"]
    , button [onClick GoToUploadPage] [text "Upload Story"]
    , button [onClick LoadIndex    ] [text "Stories"]
    ]

headerBar : String -> Html Msg
headerBar header =
  div [class "row", class "header-bar"]
    [ div [class "col-sm"]
        [ h1 [class "text-center"] [text header] ]
    ]

mdView : String -> Html Msg
mdView md = div [ class "mdView", class "row"]
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

uploadView : List (Html Msg)
uploadView = [ input [ placeholder "Story title", onInput UpdateStoryTitle ] [ text "foo" ] 
             , input [ placeholder "Text", onInput UpdateStoryText ] [ text "bar" ] 
             , button [onClick UploadStory] [text "Upload"]
             ]
