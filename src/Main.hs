{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Lens                 ((&), (.~), (<&>), (?~))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (liftResourceT, runResourceT)

import           Data.Conduit                 (($$+-))

import qualified Data.Text.Internal           as Text
import qualified Network.Google               as Google
import           Network.Google.Auth          (Auth, Credentials (..),
                                               initStore)
import           Network.Google.Auth.Scope    (AllowScopes (..), concatScopes)
import qualified Network.Google.BigQuery      as BigQuery
import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)
import           System.IO                    (stdout)

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

makeDataSet :: Text.Text -> Text.Text -> BigQuery.DataSet
makeDataSet projectName dataSetName =
  let dataSetRef = ((BigQuery.dsrProjectId ?~ projectName) . (BigQuery.dsrDataSetId ?~ dataSetName)) BigQuery.dataSetReference
  in (BigQuery.dsDataSetReference ?~ dataSetRef) BigQuery.dataSet


-- https://github.com/brendanhay/gogol/blob/develop/gogol-bigquery/gen/Network/Google/BigQuery/Types/Product.hs#L1022
insertDataSet :: Text.Text -> Text.Text -> IO BigQuery.DataSet
insertDataSet projectName dataSetName = do
    lgr  <- Google.newLogger Google.Debug stdout
    m <- liftIO (newManager tlsManagerSettings) :: IO Manager
    c <- Google.getApplicationDefault m
    -- Create a new environment which will discover the appropriate
    -- AuthN/AuthZ credentials, and explicitly state the OAuth scopes
    -- we will be using below, which will be enforced by the compiler:
    env  <- Google.newEnvWith c lgr m <&> (Google.envScopes .~ BigQuery.bigQueryScope)
    runResourceT . Google.runGoogle env $ Google.send $ BigQuery.dataSetsInsert (makeDataSet projectName dataSetName) projectName


getUserToken cres =
  case cres of
    FromUser u -> Just u
    _          -> Nothing

isUserCred :: Credentials s -> Bool
isUserCred cres =
  case cres of
    FromUser u -> True
    _          -> False

isClientCred :: Credentials s -> Bool
isClientCred cres =
  case cres of
    FromClient _ _ -> True
    _              -> False

main :: IO ()
main = print "wtf"
