{-# LANGUAGE OverloadedStrings #-}

module Server.ParserSpec where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy as BS (take)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (isJust)
import Server.Parser
import Server.Server (serve)
import Test.Hspec

import Board (Move(..))
import Coord
import Piece (Kind(..))
import Shogi (Result(Even))
import Shogi.Const (initialShogi)
import Board.AbilityProxy
import Board.Effector
import Board.Mover
import Board.MoverPredicator
import Board.Slicer
import Shogi.Judge
import Rule

jsonNormalRules =
  "{\"rule\":{\"AbilityProxy\": \"normal\", \"Effector\": \"normal\", \"Mover\": \"normal\", \"MoverPredicator\": \"normal\", \"Slicer\": \"normal\", \"Judge\": \"normal\"},\"shogi\":{\"color\":0,\"board\":[[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"KA\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{\"color\":0,\"kind\":\"HI\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"OU\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"OU\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"HI\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{\"color\":0,\"kind\":\"KA\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}]],\"hands\":[{},{}]},\"move\":{\"from\":{\"x\":8,\"y\":8},\"to\":{\"x\":7,\"y\":7}}}"
jsonRulesVariations =
  "{\"rule\":{\"AbilityProxy\": \"annan\", \"Effector\": \"nuclear\", \"Mover\": \"normal\", \"MoverPredicator\": \"madras\", \"Slicer\": \"donut\", \"Judge\": \"mate\"},\"shogi\":{\"color\":0,\"board\":[[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"KA\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{\"color\":0,\"kind\":\"HI\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"OU\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"OU\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"HI\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{\"color\":0,\"kind\":\"KA\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}]],\"hands\":[{},{}]},\"move\":{\"from\":{\"x\":8,\"y\":8},\"to\":{\"x\":7,\"y\":7}}}"

spec :: Spec
spec = do
  describe "Request" $ do
    it "initial - (7,7) - (2,7). Usual shogi"$ parse jsonNormalRules `shouldSatisfy` isJust
    it "initial - (7,7) - (2,7). Shogi variation"$ parse jsonRulesVariations `shouldSatisfy` isJust
  describe "Move" $ do
    it "7776" $
      decode "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6}}" `shouldBe`
      Just (Move (Coord 7 7) (Coord 7 6) False)
    it "7776 promote:false" $
      decode
        "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":false}" `shouldBe`
      Just (Move (Coord 7 7) (Coord 7 6) False)
    it "7776 promote:true" $
      decode
        "{\"from\":{\"x\":7,\"y\":7},\"to\":{\"x\":7,\"y\":6},\"promote\":true}" `shouldBe`
      Just (Move (Coord 7 7) (Coord 7 6) True)
    it "76FU" $
      decode "{\"piece\":\"FU\",\"to\":{\"x\":7,\"y\":6}}" `shouldBe`
      Just (Put (Coord 7 6) FU)
  describe "Rule"$ do
    it "parse rule" $
      decode
        "{\"AbilityProxy\": \"annan\", \"Effector\": \"nuclear\", \"Mover\": \"normal\", \"MoverPredicator\": \"madras\", \"Slicer\": \"donut\", \"Judge\": \"mate\"}" `shouldBe`
        Just (RuleConfig{
          abilityProxy = annanAbilityProxy,
          effector = nuclearEffector,
          mover = normalMover,
          moverPredicator = madrasMoverPredicator,
          slicer = donutSlicer,
          judge = mateJudge
        })
  describe "Response" $ do
    let nextMove1 = Move (Coord 7 7) (Coord 7 6) False
    let responseString =
          unpack $ stringify $ Just $ Response initialShogi $ Right [nextMove1]
    it "should contain next moves" $
      responseString `shouldContain` "\"next\":{\"Right\":[" ++
      unpack (encode nextMove1) ++ "]}"
    it "should contain newShogi" $
      responseString `shouldContain` "\"newShogi\":{"
    it "should contain color" $ responseString `shouldContain` "\"color\":0"
    it "should contain hands" $
      responseString `shouldContain` "\"hands\":[{},{}]"
    it "should contain board" $ responseString `shouldContain` "\"board\":["
  describe "serve" $ do
    it "initial - (7,7) - (2,7). Usual shogi" $
      BS.take 8 (serve jsonNormalRules) `shouldBe` "{\"next\":"
    it "initial - (7,7) - (2,7). Shogi variation" $
      BS.take 7 (serve jsonRulesVariations) `shouldBe` "{\"tag\":"