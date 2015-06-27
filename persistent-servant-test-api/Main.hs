 {-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Either
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as TE
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           Network.Wai
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
    Id sql=key
    desc Text Maybe
    deriving Show
|]

-- TODO: This is probably horrible performance-wise for Text -> Lazy.ByteString
instance MimeRender PlainText Test where
  mimeRender Proxy (Test desc) = case desc of
    Just d -> TE.encodeUtf8 (LT.fromStrict d)

type TestAPI = "test" :> Capture "test_id" Int :> Get '[PlainText] Test

testAPI :: Proxy TestAPI
testAPI = Proxy

server :: ConnectionPool -> Server TestAPI
server pool = getTestById pool

app :: ConnectionPool -> Application
app pool = serve testAPI (server pool)

getTestById :: MonadIO m => ConnectionPool -> t -> m (Test)
getTestById pool id = do
  mtest <- liftIO . flip runSqlPersistMPool pool $ selectFirst [] []
  case mtest of
    Just (Entity testId test) -> return (test)
    Nothing -> error "TODO: error code/not found message"


main = runNoLoggingT . withMySQLPool dbInfo 400 $ \pool -> do
         liftIO $ run 8080 (app pool)
  where dbInfo = defaultConnectInfo { connectUser = "cody" }





-- main = runNoLoggingT . withMySQLPool defaultConnectInfo { connectUser = "cody" } 10 $ \pool -> liftIO $ do
--   test <- liftIO $ flip runSqlPersistMPool pool $ do
--     getTestById pool 1
--   liftIO $ print test



-- mkPerson :: ConnectionPool -> IO ()
-- mkPerson pool = do
--     liftIO $ flip runSqlPersistMPool pool $ do
--       insert $ Person "test" Nothing
--     return ()
--   where dbInfo = defaultConnectInfo { connectUser = "cody" }


-- data Test = Test { key :: Text
--                  , age  :: Maybe Int
--                  } deriving (Show, Generic)


-- instance FromJSON Test
-- instance ToJSON Test

-- type TestAPI = "test" :> Get '[JSON] [Test]
