{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson
import           Data.Bool
import           Data.Char                         (toUpper)
import qualified Data.List                         as L
import           Data.Monoid
import           Data.Proxy
import           Data.Text                         hiding (head, length, map,
                                                    null, toUpper)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           GHC.Generics
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Http.Server

import           Servant
import           Servant.Server
-- import           Snap.Util.FileServe
import           API
import           Snap

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
server :: Server API '[BasicAuthCheck (Handler App App) ()] (Handler App App)
server = return () :<|> return 100 :<|> sayhi :<|> dbl
    :<|> multi :<|> qna :<|> serveSecret
    :<|> serveDirectory "static"
  where sayhi :: Maybe Text -> [Text] -> Bool -> Handler App App Text
        sayhi nm greetings withGusto = case nm of
          Nothing -> return ("Sorry, who are you?" :: Text)
          Just n  -> do
           let modifier  = bool id T.toUpper withGusto
               greetPart
                 | null greetings        = "Hi, "
                 | length greetings == 1 = L.head greetings <> ", "
                 | otherwise             = T.intercalate ", " (L.init greetings)
                                       <> ", and " <> L.last greetings <> ", "
           return . modifier $ greetPart <> n
        dbl x = if x `elem` [4,13]
                then throwError $ err500 { errBody = "No unlucky numbers please" }
                else return $ x * 2
        multi = return . bool "Box unchecked" "Box Checked"
        qna q = do
          liftIO $ do
            putStrLn $ "qna got: " ++ show q
            T.putStrLn $ unQuestion q
          return $ Answer $ unQuestion q
        serveSecret _ = do
          req <- getRequest
          liftIO $ putStrLn (show req)
          return 101

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Handler App App ()
test = serveSnapWithContext testApi
       (BasicAuthCheck (\_ -> return @(Handler App App) (Authorized ())) :. EmptyContext) server

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "example" Nothing $ do
  addRoutes [("", test)
            ,("", serveDirectory "static")
            ]
  return App

-- Put this all to work!
main :: IO ()
main = serveSnaplet mempty initApp
