{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson
import           Data.Bool
import           Data.Char (toUpper)
import           Data.CaseInsensitive (CI(..),mk)
import qualified Data.List as L
import           Data.Monoid
import           Data.Proxy
import           Data.Text hiding (length, null, map, head, toUpper)
import           GHC.Generics
import           Network.HTTP.Types
import           Snap.Http.Server
import           Snap.Core
import           Servant.Server.Internal.SnapShims

import           Servant -- hiding (serveDirectory)
import           Servant.Server
-- import           Snap.Util.FileServe
import           API
import Snap

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

testApi :: Proxy API
testApi = Proxy

data App = App

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Server API (Handler App App)
server = return () :<|> return 100 :<|> sayhi :<|> dbl :<|> multi :<|> myHeader :<|> serveDirectory "static"
  where sayhi nm greetings withGusto = case nm of
          Nothing -> return ("Sorry, who are you?" :: String)
          Just n  -> do
           let modifier  = bool id (map toUpper) withGusto
               greetPart
                 | null greetings        = "Hi, "
                 | length greetings == 1 = L.head greetings ++ ", "
                 | otherwise             = L.intercalate ", " (L.init greetings)
                                       ++ ", and " ++ L.last greetings ++ ", "
           return . modifier $ greetPart ++ n
        dbl x = return $ x * 2
        multi = return . bool "Box unchecked" "Box Checked"
        myHeader = return (Headers 222 (buildHeadersTo [(mk "myheader" :: HeaderName,"333")]))
          -- modifyResponse (addHeader (CI "myheader" ))

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application (Handler App App)
test = serve testApi server

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "example" Nothing $ do
  addRoutes [("", applicationToSnap test)
            ,("", serveDirectory "static")
            ]
  return App

-- Put this all to work!
main :: IO ()
main = serveSnaplet mempty initApp
