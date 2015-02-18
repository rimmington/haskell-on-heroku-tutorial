module Server where

import Control.Concurrent.STM
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import API

server :: Text -> TVar [Note] -> Server NoteAPI
server home notes =
         return home
    :<|> getNotes notes
    :<|> postNote notes

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" pack $
                 lookup "TUTORIAL_HOME" env
    notes <- emptyNotes
    run port $ serve noteAPI $ server home notes
