{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Story
( allExampleStories
, lookupStoryByID
, Story(..)
, StoryMeta(..)
, ErrMsg(..)
, story
)
where
import GHC.Generics
import Data.Text hiding (find)
import Data.List (find)

import Lens.Micro.TH (makeLenses)
-- import Lens.Micro ((&), (%~), (.~), (^.))
import Lens.Micro ((^.))

import Data.Aeson (ToJSON, toJSON, object, (.=))

type StoryID = Int

data StoryMeta =
  StoryMeta { _storyID    :: StoryID
            , _storyTitle :: Text } deriving (Show, Generic)

instance ToJSON StoryMeta where
  toJSON (StoryMeta sid stitle) =
    object [ "storyID"    .= sid
           , "storyTitle" .= stitle ]

makeLenses ''StoryMeta

data Story =
  Story { _meta       :: StoryMeta
        , _storyText  :: Text      } deriving (Show, Generic)

instance ToJSON Story where
  toJSON (Story smeta stext) =
    object [ "storyMeta" .= toJSON smeta
           , "storyText" .= stext        ]

makeLenses ''Story

data ErrMsg = ErrMsg { _errMsg :: Text }
  deriving (Show, Generic)

instance ToJSON ErrMsg where
  toJSON (ErrMsg msg) = object ["error" .= msg]

makeLenses ''ErrMsg

exampleStory1 = story 1 "Attack of the mutant custard" "The end"
exampleStory2 = story 2 "Jan's Tomatoes" "They grew"

allExampleStories = [exampleStory1, exampleStory2]

story sid stitle stext = 
  Story { _meta = StoryMeta { _storyID    = sid 
                            , _storyTitle = stitle }
        , _storyText = stext }

lookupStoryByID :: StoryID -> [Story] -> Maybe Story
lookupStoryByID = find . storyIDEquals

storyIDEquals :: StoryID -> Story -> Bool
storyIDEquals sid stry = (stry ^. (meta . storyID)) == sid

