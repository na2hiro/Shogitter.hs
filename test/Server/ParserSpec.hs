{-# LANGUAGE OverloadedStrings #-}
module Server.ParserSpec where

import Test.Hspec
import Server.Parser
import Server.Server(serve)
import Data.Maybe(isJust)
import Data.Aeson(decode, encode)
import Data.ByteString.Lazy as BS(take)
import Data.ByteString.Lazy.Char8(unpack)

import Shogi(Result(Even))
import Shogi.Const(initialShogi)
import Coord
import Board(Move(..))
import Piece(Kind(..))

json = "{\"rule\":{},\"shogi\":{\"color\":0,\"data\":[[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"KA\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{\"color\":0,\"kind\":\"HI\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"OU\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"OU\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"HI\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{\"color\":0,\"kind\":\"KA\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}]],\"hands\":[{},{}]},\"move\":{\"from\":{\"x\":8,\"y\":8},\"to\":{\"x\":7,\"y\":7}}}"

spec :: Spec
spec = do
    describe "Request"$ do
        it "initial - (7,7) - (2,7)"$ parse json `shouldSatisfy` isJust
    describe "Move"$ do
        it "7776"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6}}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) False)
        it "7776 promote:false"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":false}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) False)
        it "7776 promote:true"$ decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":true}" `shouldBe` Just (Move (Coord 7 7) (Coord 7 6) True)
        it "76FU"$ decode "{\"piece\":\"FU\",\"to\":{\"x\":7,\"y\":6}}" `shouldBe` Just (Put (Coord 7 6) FU)
    describe "Response"$ do
        let nextMove1 = Move (Coord 7 7) (Coord 7 6) False
        let responseString = unpack$ stringify$ Just$ Response initialShogi$ Right [nextMove1]
        it "should contain next moves"$ responseString `shouldContain` "\"next\":{\"Right\":[" ++ unpack (encode nextMove1) ++ "]}"
        it "should contain newShogi"$ responseString `shouldContain` "\"newShogi\":{"
        it "should contain color"$ responseString `shouldContain` "\"color\":0"
        it "should contain hands"$ responseString `shouldContain` "\"hands\":[{},{}]"
        it "should contain board"$ responseString `shouldContain` "\"board\":["
    describe "serve"$ do
        it "initial - (7,7) - (2,7)"$ BS.take 8 (serve json) `shouldBe` "{\"next\":"
