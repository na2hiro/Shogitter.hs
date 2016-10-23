{-# LANGUAGE OverloadedStrings #-}
module Server.ParserSpec where

import Test.Hspec
import Server.Parser
import Data.Maybe(isJust)
import Data.Aeson(decode)

import Coord
import Board(Move(..))
import Piece(Kind(..))

spec :: Spec
spec = do
    describe "Request"$ do
        let json = "{\"rule\":{},\"shogi\":{\"color\":0,\"data\":[[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"KA\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{\"color\":0,\"kind\":\"HI\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"OU\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"OU\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"HI\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{\"color\":0,\"kind\":\"KA\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}]],\"hands\":[{},{}]},\"move\":{\"piece\":\"FU\",\"to\":{\"x\":1,\"y\":2}}}"
        it "initial - (7,7) - (2,7)"$ parse json `shouldSatisfy` isJust
    describe "Move"$ do
        it "7776"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6}}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) False)
        it "7776 promote:false"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":false}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) False)
        it "7776 promote:true"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":true}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) True)
        it "76FU"$ decode "{\"piece\":\"FU\",\"to\":{\"x\":7,\"y\":6}}" `shouldBe` Just (Put (Coord 7 6) FU)
