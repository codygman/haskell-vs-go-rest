{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
import           Control.Concurrent       (forkOS)
import           Control.Exception        (bracket)
import           Control.Monad            (void)
import           Data.Monoid              (mempty)
import           Data.Pool                (Pool, createPool,
                                           destroyAllResources, withResource)
import qualified Data.Text.Lazy.Encoding  as LT
import qualified Database.MySQL.Base      as MySQL
import           Database.MySQL.Simple
import           GHC.IO                   (unsafeUnmask)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  MySQL.initLibrary
  bracket mkPool destroyAllResources $ \pool ->
    Warp.runSettings (Warp.setPort 8000 . Warp.setFork forkOSWithUnmask $ Warp.defaultSettings) $
      \req resp -> do
        MySQL.initThread
        withResource pool $ \conn ->
          case pathInfo req of
            [key] -> do
              rs <- query conn "SELECT `desc` FROM `test` WHERE `key` = ?"
                (Only key)
              case rs of
                Only result : _ -> resp $
                  responseLBS
                    ok200
                    [(hContentEncoding, "text/plain")]
                    (LT.encodeUtf8 result)
                _ -> resp e404
            _ -> resp e404

  where
    mkPool = createPool (connect defaultConnectInfo { connectUser = "cody" }) close 1 60 10
    e404 = responseLBS notFound404 [] mempty
    forkOSWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ()
    forkOSWithUnmask io = void $ forkOS (io unsafeUnmask)



-- attempting to break out into functions


-- getTestById :: forall b.
--                      Request -> (Response -> IO b) -> Pool Connection -> IO b
-- getTestById req resp pool = do
--         MySQL.initThread
--         withResource pool $ \conn ->
--           case pathInfo req of
--             [key] -> do
--               rs <- query conn "SELECT `desc` FROM `test` WHERE `key` = ?"
--                 (Only key)
--               case rs of
--                 Only result : _ -> resp $
--                   responseLBS
--                     ok200
--                     [(hContentEncoding, "text/plain")]
--                     (LT.encodeUtf8 result)
--                 _ -> resp e404
--             _ -> resp e404
--         where e404 = responseLBS notFound404 [] mempty



-- main :: IO ()
-- main = do
--   MySQL.initLibrary
--   bracket mkPool destroyAllResources $ \pool ->
--     Warp.runSettings (Warp.setPort 8000 . Warp.setFork forkOSWithUnmask $ Warp.defaultSettings) $
--     (\req resp -> getTestById req resp pool)
