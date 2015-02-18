module APISpec where

import Test.Hspec

import API

spec :: Spec
spec = do
    describe "getNotes" $ do
        it "returns empty note lists" $ do
            notes <- emptyNotes
            getNotes notes `shouldReturn` []

    describe "postNote" $ do
        it "adds notes" $ do
            notes <- emptyNotes
            let note = Note "My note"
            postNote notes note
            getNotes notes `shouldReturn` [note]

main :: IO ()
main = hspec spec
