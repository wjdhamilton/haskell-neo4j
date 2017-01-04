module Database.Neo4j where

import Data.Text (Text)
import qualified Data.Text as Text

data Request

data Statement

data Transaction a = TransactionError [Text]
                   | TransactionComplete [a]
                   | Transaction { commit :: Text
                                 , results :: [a]
                                 , transaction :: Maybe Text
                                 }
                                




