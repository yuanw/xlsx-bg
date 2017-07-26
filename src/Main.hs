{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens                 ((&), (.~), (<&>), (?~))
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Control.Monad.IO.Class

import Data.Conduit (($$+-))
import Data.Text    (Text)

import System.IO (stdout)

import Network.Google.Auth   (Auth, Credentials (..), initStore)
import qualified Network.Google         as Google
import qualified Network.Google.BigQuery as BigQuery
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import           Network.Google.Auth.Scope       (AllowScopes (..),
                                                  concatScopes)


-- This will calculate the MIME type (and therefore Content-Type) of
-- the stored object based on the file extension.
--
-- You can explicitly set the desired MIME type by using:
-- {-
-- import Network.HTTP.Media ((//))
----
-- b <- sourceBody f <&> bodyContentType .~ "application" // "json"
-- ...
--
-- -}
--example :: Text -> FilePath -> IO ()


example :: IO BigQuery.ProjectList
example = do
    lgr  <- Google.newLogger Google.Debug stdout
    m <- liftIO (newManager tlsManagerSettings) :: IO Manager
    c <- Google.getApplicationDefault m

    -- Create a new environment which will discover the appropriate
    -- AuthN/AuthZ credentials, and explicitly state the OAuth scopes
    -- we will be using below, which will be enforced by the compiler:
    env  <- Google.newEnvWith c lgr m <&>
          (Google.envLogger .~ lgr)
        . (Google.envScopes .~ BigQuery.bigQueryScope)

    runResourceT . Google.runGoogle env $ Google.send BigQuery.projectsList

getUserToken cres = case cres of
  FromUser u -> Just u
  _          -> Nothing

isUserCred :: Credentials s -> Bool
isUserCred cres = case cres of
  FromUser u -> True
  _          -> False

isClientCred :: Credentials s -> Bool
isClientCred cres = case cres of
  FromClient _ _ -> True
  _          -> False

main :: IO ()
main = do
    projects <- example
    print projects


