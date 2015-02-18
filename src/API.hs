module API where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API

import qualified Data.Text.IO as T


newtype Note = Note
    { contents :: Text
    }
  deriving (Generic, Show, Eq)

instance FromJSON Note
instance ToJSON Note


emptyNotes :: IO (TVar [Note])
emptyNotes =
    newTVarIO []

getNotes :: MonadIO m => TVar [Note] -> m [Note]
getNotes notes =
    liftIO $ readTVarIO notes

postNote :: MonadIO m => TVar [Note] -> Note -> m [Note]
postNote notes note =
    liftIO $ do
      T.putStrLn $ contents note
      atomically $ do
        oldNotes <- readTVar notes
        let newNotes = note : oldNotes
        writeTVar notes newNotes
        return newNotes


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody Note :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

