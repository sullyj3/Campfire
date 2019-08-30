{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Story
( Story(..)
, StoryMeta(..)
, StoryUpload(..)
, ErrMsg(..)
)
where
import GHC.Generics
import Data.Text hiding (find)
import Data.List (find)

import Lens.Micro.TH (makeLenses)
-- import Lens.Micro ((&), (%~), (.~), (^.))
import Lens.Micro ((^.))

import Data.Aeson (FromJSON(..), ToJSON, toJSON, object, (.=), withObject, (.:))
import Database.PostgreSQL.Simple (ToRow)

type StoryID = Int

-- the main story type
data Story =
  Story { _storyID    :: StoryID
        , _storyTitle :: Text
        , _storyText  :: Text      } deriving (Show, Generic)

instance ToJSON Story where
  toJSON (Story sid stitle stext) =
    object [ "storyID"    .= sid
           , "storyTitle" .= stitle
           , "storyText"  .= stext ]

makeLenses ''Story

-- Just the story metadata, without the text. Used to send out to the index page
data StoryMeta =
  StoryMeta { _sm_storyID    :: StoryID
            , _sm_storyTitle :: Text } deriving (Show, Generic)

instance ToJSON StoryMeta where
  toJSON (StoryMeta sid stitle) =
    object [ "storyID"    .= sid
           , "storyTitle" .= stitle ]

makeLenses ''StoryMeta

-- Has no id yet, as it has not yet been stored in the database. No ToJSON instance necessary
data StoryUpload =
  StoryUpload { _su_storyTitle :: Text
              , _su_storyText  :: Text } deriving (Show, Generic, ToRow)

instance FromJSON StoryUpload where
  parseJSON = withObject "StoryUpload" $ \v -> StoryUpload
        <$> v .: "storyTitle"
        <*> v .: "storyText"

makeLenses ''StoryUpload

data ErrMsg = ErrMsg { _errMsg :: Text }
  deriving (Show, Generic)

instance ToJSON ErrMsg where
  toJSON (ErrMsg msg) = object ["error" .= msg]

makeLenses ''ErrMsg
