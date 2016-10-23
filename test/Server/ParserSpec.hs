{-# LANGUAGE OverloadedStrings #-}
module Server.ParserSpec where

import Test.Hspec
import Server.Parser
import Data.Maybe(isJust)

spec :: Spec
spec = do
    describe "parse"$ do
        let json = "{\"rule\":{},\"shogi\":{\"color\":0,\"data\":[[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"KA\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{\"color\":0,\"kind\":\"HI\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"OU\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"OU\"}],[{\"color\":1,\"kind\":\"KI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KI\"}],[{\"color\":1,\"kind\":\"GI\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{},{},{\"color\":0,\"kind\":\"GI\"}],[{\"color\":1,\"kind\":\"KE\"},{\"color\":1,\"kind\":\"HI\"},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{\"color\":0,\"kind\":\"KA\"},{\"color\":0,\"kind\":\"KE\"}],[{\"color\":1,\"kind\":\"KY\"},{},{\"color\":1,\"kind\":\"FU\"},{},{},{},{\"color\":0,\"kind\":\"FU\"},{},{\"color\":0,\"kind\":\"KY\"}]],\"hands\":[{},{}]},\"move\":{\"piece\":\"FU\",\"to\":{\"x\":1,\"y\":2}}}"
        it "initial - (7,7) - (2,7)"$ parse json `shouldSatisfy` isJust
