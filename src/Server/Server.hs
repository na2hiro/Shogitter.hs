module Server.Server where

import Data.ByteString.Lazy(ByteString)

import Shogi(Shogi, doMove, judge, getMovesShogi, board)
import Server.Parser(stringify, parse, Request(..), Response(..), Rule(..))

serve :: ByteString -> ByteString
serve json = stringify$ process =<< parse json

process :: Request -> Maybe Response
process (Request rule shogi move) = do
    let shogi' = injectRule rule shogi
    nextShogi <- doMove move shogi'
    let movesOrJudgment = case judge nextShogi of
            Nothing -> Right$ getMovesShogi nextShogi
            Just r -> Left r
    return$ Response (board nextShogi) movesOrJudgment

injectRule :: Rule -> Shogi -> Shogi
injectRule = fail "injectRule"
