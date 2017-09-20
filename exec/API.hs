{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Text        (Text)
import           Servant.API

newtype Question = Question { unQuestion :: Text } deriving (Show)

instance ToJSON Question where
  toJSON (Question txt) = object ["question" .= txt]

instance FromJSON Question where
  parseJSON (Object v) = Question <$> v .: "question"
  parseJSON x          = typeMismatch "Couldn't find key 'question'" x

newtype Answer = Answer { unAnswer :: Text } deriving (Show)

instance ToJSON Answer where
  toJSON (Answer txt) = object ["answer" .= txt]

instance FromJSON Answer where
  parseJSON (Object v) = Answer <$> v .: "answer"
  parseJSON x          = typeMismatch "Couldn't find key 'answer'" x


-- | API spec for server, client, and docs
type API = "getunit" :> Get '[JSON] ()
      :<|> "getint"  :> Get '[JSON] Int
      :<|> "sayhi"   :> QueryParam  "username" Text
                     :> QueryParams "greetings" Text
                     :> QueryFlag   "gusto"
                     :> Get '[JSON] Text
      :<|> "double" :> ReqBody '[JSON] Double
                    :> Post '[JSON] Double
      :<|> "a" :> "b" :> QueryFlag "gusto" :> Get '[JSON] Text
      :<|> "qna" :> ReqBody '[JSON] Question
                 :> Post '[JSON] Answer
      :<|> "secret" :> BasicAuth "realm" () :> Get '[JSON] Int
      :<|> Raw

type GET = Get '[JSON] ()

-- Imported the comprehensive API example for testing.
-- https://github.com/haskell-servant/servant/blob/master/servant/src/Servant/API/Internal/Test/ComprehensiveAPI.hs
type ComprehensiveAPI =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
-- Raw :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
  Get '[JSON] (Headers '[Header "foo" Int] ()) :<|>
  "foo" :> GET :<|>
  Vault :> GET :<|>
  Verb 'POST 204 '[JSON] () :<|>
  Verb 'POST 204 '[JSON] Int
-- This one isn't in scope
--  :<|> WithNamedContext "foo" '[] GET
