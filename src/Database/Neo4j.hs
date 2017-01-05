{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Neo4j where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import Data.Text.Encoding as TE
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import GHC.Generics
import Network.URL (URL(..), URLType(..), Host(..), Protocol(..))
import qualified Network.URL as URL
import Network.Wreq (Response(..))
import qualified Network.Wreq as Wreq

-- | The Graphable class defines functions that convert a ByteString in the 
-- format of a result to an instance
class Graphable a where




-- | The Result class represents a Haskell representation of the bare response
-- from the Neo4j server
data Result = Result


-- | Represents the content of a request that can be sent to the Neo4j instance.
data Request = Request { auth :: Maybe (Text, Text) -- ^ tuple of (username, password)
                       , host_url :: URL -- ^ The url of the Neo4j instance
                       , statements :: [Statement] -- ^ The list of statements to execute
                       }


-- | Represents a Statement to be executed on the server
data Statement = Statement { statement :: Text -- ^ The statement to execute
                           , params :: HashMap Text Text -- ^ Map of params to be used in the statement
                           } deriving (Generic)

instance ToJSON Statement


-- | Represents the outcome of a Request. Transactions can be in one of three
-- states relating to the success of the operation on the server and its 
-- subsequent response.
data Transaction a = TransactionError [Text] -- ^ Indicates that the Transaction has failed

                   -- | The transaction is still open. 
                   | Transaction { commit_url :: URL -- ^ The url to be called to commit the transaction
                                 -- | The results so far
                                 , results :: [a]
                                 -- | The expiry of the transaction. Nothing if the transaction
                                 -- cannot be updated
                                 , transaction :: Maybe Text 
                                 , transaction_url :: URL -- ^ The url to use to progress the transaction
                                 }
                   | TransactionComplete [a]
                                

-- | Returns the URLType representing the host Neo4j instance
dbHost :: URLType
dbHost = Absolute (Host (HTTP True) "http://localhost" (Just 7474))


-- | The address of the user with username (n)
userAddress :: Text -> URL
userAddress n = URL dbHost ("/user/" <> Text.unpack n) []

-- TODO: There is a problem here. The request process is necessarily in IO. But
-- the overall process is supposed to be using the Transaction. I think that 
-- this may mean that we ought to create a Monad Transformer like EitherT for the 
-- Transaction instance in order that we can lift the values out of the IO 
-- context and into the transaction. This is because, in this case, the
-- IO element is an implementation detail. We don't actually care that the 
-- transaction is an IO at the bottom, just that it's a Transaction so let's 
-- try and get rid of that. 

-- | Send a request to the server
sendRequest :: Request -> IO (Transaction a)
sendRequest (Request a h s) = do
          let url = URL.exportURL h
          let options = case a of
                             Nothing -> Wreq.defaults
                             Just (user, pass) ->
                               Wreq.defaults & Wreq.header "Authorization" .~ [auth]
                               where auth = Base64.encode $ TE.encodeUtf8 (user <> ":" <> pass)
          r <- Wreq.postWith options url (toJSON s)
          fromResponse r

-- Add the auth header
-- Build a json object from the statements
-- post the request
-- scrutinise the response
-- Create an appropriate Transaction object from the response

fromResponse :: Response ByteString -> IO (Transaction a)
fromResponse = undefined


-- | Send a request to the server using a Transaction as a starting point. This
-- looks pretty monadic. 
(>~>) :: Transaction a -> (a -> Transaction b) -> Transaction b
(>~>) = undefined
