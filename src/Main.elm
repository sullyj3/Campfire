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


type alias StoryMeta a =
  { a | storyID    : StoryID
      , storyTitle : String }

type alias Story = StoryMeta { storyText : String }

type alias StoryUpload = { storyTitle : String, storyText  : String }

type UploadPageState = EnteringStory StoryUpload
                     | UploadingStory StoryUpload
                     | UploadError Http.Error
                     | UploadSuccess

type IndexPageState
  = LoadingIndex
  | LoadedIndex (List (StoryMeta {}))
  | IndexError

type CurrentPage
  = UploadPage UploadPageState
  | IndexPage IndexPageState
  | StoryPage StoryPageState

type StoryPageState
  = LoadingStory StoryID
  | LoadedStory Story
  | StoryNotFound

type alias Model = { apiURL : String
                   , logoUrl : String
                   , page : CurrentPage
                   }

-- update2 : (Model, Msg) -> (Model, Cmd Msg)
-- update2 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of

  -- USER INPUT
  (LoadStory id) -> loadStory id model
  LoadIndex      -> loadIndex model
  GoToUploadPage -> goToUploadPage model
  UploadStory su   -> uploadStory su model

  (UpdateStoryTitle title) ->
    case model.page of
      UploadPage (EnteringStory storyUpload) ->
        ({model | page = UploadPage <| EnteringStory {storyUpload | storyTitle = title }}, Cmd.none)
      _ -> (model, Cmd.none) -- TODO - is there a way to get rid of this?
  (UpdateStoryText text) ->
    case model.page of
      UploadPage (EnteringStory storyUpload) ->
        ({model | page = UploadPage <| EnteringStory {storyUpload | storyText = text }}, Cmd.none)
      _ -> (model, Cmd.none) -- TODO - is there a way to get rid of this?

  -- SERVER RESPONSES
  (GotIndex metas) ->
    ({ model | page = IndexPage <| LoadedIndex metas }, Cmd.none )
  IndexErrorMsg ->
    ({model | page = IndexPage IndexError }, Cmd.none)
  (GotStory s) ->
    ({ model | page = StoryPage <| LoadedStory s }, Cmd.none )
  StoryErrorMsg ->
    ({model | page = StoryPage StoryNotFound }, Cmd.none)
  (UploadStoryResponse rslt) ->
    case rslt of
      Ok () -> ({ model | page = UploadPage UploadSuccess }, Cmd.none)
      Err e -> ({ model | page = UploadPage (UploadError e) }, Cmd.none)
  

loadStory : Int -> Model -> (Model, Cmd Msg)
loadStory id model = 
  ({ model | page = StoryPage <| LoadingStory id }, getStory model.apiURL id )

loadIndex : Model -> (Model, Cmd Msg)
loadIndex model = ({ model | page = IndexPage LoadingIndex }, getIndex model.apiURL )

goToUploadPage : Model -> (Model, Cmd Msg)
goToUploadPage model = ({ model | page = UploadPage emptyStoryUpload }, Cmd.none )

uploadStory : StoryUpload -> Model -> (Model, Cmd Msg)
uploadStory su model = 
  ( {model | page = UploadPage <| UploadingStory su}
  , postStory (model.apiURL ++ "/upload") su )


testMd = """
## Header 2 ##

### Here's a list: ###

1. List item 1

   continued

2. List item 2
"""

type alias Flags = { api_url : String
                   , logo_url : String}


init : Flags -> (Model, Cmd Msg)
init flags = ( { apiURL = flags.api_url
               , logoUrl = flags.logo_url
               , page   = IndexPage LoadingIndex
               }
             , getIndex flags.api_url
             )

-- UPDATE -------------------------------------------------------------------

type Msg =
         -- User input
           LoadStory StoryID
         | LoadIndex
         | GoToUploadPage
         | UploadStory StoryUpload

         | UpdateStoryTitle String
         | UpdateStoryText  String

         -- Server responses
         | GotIndex (List (StoryMeta {}))
         | IndexErrorMsg

         | GotStory Story
         | StoryErrorMsg

         | UploadStoryResponse (Result Http.Error ())


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

postStory : String -> StoryUpload -> Cmd Msg
postStory apiURL stry = Http.post
  { url = apiURL
  , body = Http.jsonBody <| encodeStoryUpload stry
  , expect = Http.expectWhatever UploadStoryResponse }

emptyStoryUpload : UploadPageState
emptyStoryUpload = EnteringStory { storyTitle="", storyText="" }

updateStoryTitle : String -> StoryUpload -> StoryUpload
updateStoryTitle t su = { su | storyTitle = t }

updateStoryText : String -> StoryUpload -> StoryUpload
updateStoryText t su = { su | storyText = t }

decodeIndex : Decoder (List (StoryMeta {}))
decodeIndex = list decodeStoryMeta

decodeStoryMeta : Decoder (StoryMeta {})
decodeStoryMeta = map2 (\ id title -> {storyID=id, storyTitle=title})
  (field "storyID" int)
  (field "storyTitle" string)

decodeStory : Decoder Story
decodeStory = map3 (\id title text -> {storyID=id, storyTitle=title, storyText=text})
  (field "storyID" int)
  (field "storyTitle" string)
  (field "storyText" string)

encodeStoryUpload : StoryUpload -> Enc.Value
encodeStoryUpload stry = Enc.object
    [ ("storyTitle", Enc.string stry.storyTitle)
    , ("storyText" , Enc.string stry.storyText )
    ]

handleHttpGetIndex : Result Http.Error (List (StoryMeta {})) -> Msg
handleHttpGetIndex r = case r of
  Ok entries -> GotIndex entries
  Err e -> IndexErrorMsg

handleStoryHttpResult : Result Http.Error Story -> Msg
handleStoryHttpResult r = case r of
  Ok s  -> GotStory s
  Err e -> StoryErrorMsg

-- VIEW -------------------------------------------------------------------

view : Model -> Document Msg
view model =
  Document "Campfire"
    [ navBar model.logoUrl
    , bodyView model
    , footer
    ]

footer = div [id "footer"] [iconsBy]

bodyView : Model -> Html Msg
bodyView model =
    div [class "container"]
      <| case model.page of
        UploadPage ups -> uploadView ups
        IndexPage  ips -> [ indexView  ips ]
        StoryPage  sps -> storyView  sps

        --   UploadPage _         -> uploadView
        --   LoadingIndex         -> [ text "loading index" ]
        --   LoadingStory id      -> [ text <| "loading story id " ++ fromInt id ]
        --   LoadedIndex entries  -> indexView entries
        --   LoadedStory stry    -> storyView stry
        --   StoryNotFound _      -> [ text "couldn't find the story!" ]
           -- CouldntConnect       -> [ text "couldn't reach the backend" ]

logo : String -> Html Msg
logo logoUrl = img [src logoUrl, alt "Bonfire logo", id "logo"] []

navBar : String -> Html Msg
navBar logoUrl =
  nav [class "nav-bar"]
    [ logo logoUrl
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

storyView : StoryPageState -> List (Html Msg)
storyView sps = case sps of
  LoadingStory id -> [text "loading story"]
  LoadedStory s -> storyLoadedView s
  StoryNotFound -> [text "Couldn't find that story!"]

storyLoadedView : Story -> List (Html Msg)
storyLoadedView { storyID, storyTitle, storyText } =
    [ headerBar storyTitle
    , mdView storyText ]

storyMetaLinkView : StoryMeta a -> Html Msg
storyMetaLinkView sm =
  button [onClick <| LoadStory sm.storyID] [storyMetaView sm]

storyMetaView : StoryMeta a -> Html Msg
storyMetaView {storyID, storyTitle} =
  text <| fromInt storyID ++ ": " ++ storyTitle


indexView : IndexPageState -> (Html Msg)
indexView ips = case ips of
  LoadingIndex -> text "loading index"
  LoadedIndex storyList ->  indexLoadedView storyList 
  IndexError -> text "Error fetching the index"

indexLoadedView : List (StoryMeta a) -> (Html Msg)
indexLoadedView entries = container "index-container" 
  [  h1 [] [text "Stories"]
  , ul [] (map (li [] << singleton << storyMetaLinkView) entries)  ]

uploadView : UploadPageState -> List (Html Msg)
uploadView ups = case ups of
  (EnteringStory su) -> [storyEditor su ]
  (UploadingStory _) -> [ text "Uploading..." ]
  (UploadError _)    -> [ text "Error uploading story" ]
  (UploadSuccess)    -> [ text "Successfully uploaded!" ]

container : String -> List (Html Msg) -> Html Msg
container cls elts = div [ class cls ] elts

storyEditor : StoryUpload -> Html Msg
storyEditor su = container "storyEditor"
  [ container "story-title-input-container"
              [ input [ placeholder "Story title"
                      , onInput UpdateStoryTitle
                      , id "story-title-input" ]
                      [ text su.storyTitle ] ]
  , container "story-text-input-container" 
              [ textarea [ placeholder "Story text here"
                         , onInput UpdateStoryText
                         , id "story-text-input"]
                         [ text su.storyText ] ] 
  , button [onClick <| UploadStory su] [text "Upload"]
  ]

-- <div>Icons made by <a href="https://www.flaticon.com/authors/freepik" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/"         title="Flaticon">www.flaticon.com</a></div>
iconsBy : Html Msg
iconsBy = div [] [ text "Icons made by "
                 , a [ href "https://www.flaticon.com/authors/freepik"
                     , title "Freepik" ]
                     [ text "Freepik" ] 
                 , text " from "
                 , a [ href "https://www.flaticon.com/"
                     , title "Flaticon" ]
                     [ text "www.flaticon.com" ] 
                 ]

