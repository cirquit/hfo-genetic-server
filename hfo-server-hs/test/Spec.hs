{-# LANGUAGE ViewPatterns #-}

import Test.Hspec
import System.Directory
import Data.List  (sort)
import Data.Aeson

import Genetic
import HFO
import HFO.ToFlags
import HFO.StateParser


main :: IO ()
main = hspec $ do
    describe "HSpec tests for hfo-server-hs" $ do
        describe "HFO" $ do
            describe "HFO.Agent" $ do

                it "offense agent toFlags instance" $ do
                    let offense = Offense { offActionDist     = ([(Move,  25), (Intercept, 30), (Catch, 40), (NoOp, 5)], [])
                                          , offBallActionDist = ([(Shoot, 40), (Dribble,   60)],                         [])
                                          }
                        agent = AgentConf { teamName = "base_left"
                                          , isGoalie = False
                                          , aseed    = 123
                                          , episodes = 1
                                          , actions  = Left offense
                                          }
                    toFlags_ agent `shouldBe` ["--team","base_left","--episodes","1","--seed","123","--offactions","25","30","40","5","40","60"]

                it "defense agent toFlags instance" $ do
                    let defense = Defense { defActionDist = ([(Move,  45), (Intercept, 0), (Catch, 20), (NoOp, 35)], []) }
                        agent = AgentConf { teamName = "base_right"
                                          , isGoalie = True
                                          , aseed    = 456
                                          , episodes = 4
                                          , actions  = Right defense
                                          }
                    toFlags_ agent `shouldBe` ["--team","base_right","--goalie","--episodes","4","--seed","456","--defactions","45","0","20","35"]

            describe "HFO.Server" $ do
                it "server toFlags instance" $ do
                    let server = ServerConf { showMonitor      = True
                          , trials           = -1
                          , frames           = -1
                          , framespertrial   = 1000
                          , untouchedTime    = 50
                          , offenseAgents    = 4
                          , defenseAgents    = 4
                          , offenseNpcs      = 0
                          , defenseNpcs      = 0
                          , offenseTeam      = Just Base
                          , defenseTeam      = Just Base
                          , standartPace     = False
                          , port             = 6000
                          , noLogging        = False
                          , logdir           = "log/"
                          , recordLogs       = False
                          , giveBallToPlayer = 0
                          , fullState        = False
                          , seed             = 31415926
                          , messageSize      = 128
                          , ballMinX         = 0
                          , ballMaxX         = 0.2
                          }

                    toFlags_ server `shouldBe` ["--frames-per-trial","1000"
                                               ,"--port","6000"
                                               ,"--seed","31415926"
                                               ,"--frames","-1"
                                               ,"--log-dir","log/"
                                               ,"--trials","-1"
                                               ,"--ball-x-max","0.2"
                                               ,"--ball-x-min","0.0"
                                               ,"--untouched-time","50"
                                               ,"--offense-agents","4"
                                               ,"--defense-agents","4"
                                               ,"--offense-team","base"
                                               ,"--defense-team","base"
                                               ,"--offense-on-ball","0"
                                               ,"--message-size","128"
                                               ]
            describe "HFO.Agent.Data" $ do
                describe "JSON Encoding / Decoding" $ do
                    it "DefenseTeam" $ do
                        let testDefenseTeam = DefenseTeam { goalie     = testDefaultDefense
                                                          , dp2        = testDefaultDefense
                                                          , dp3        = testDefaultDefense
                                                          , dp4        = testDefaultDefense
                                                          , defFitness = (0, [Just Goal, Nothing, Just ServerDown, Just OutOfTime])}

                        eitherDecode (encode testDefenseTeam) `shouldBe` (Right testDefenseTeam)

                    it "OffenseTeam" $ do
                        let testOffenseTeam = OffenseTeam { op1        = testDefaultOffense
                                                          , op2        = testDefaultOffense
                                                          , op3        = testDefaultOffense
                                                          , op4        = testDefaultOffense
                                                          , offFitness = (25, [Nothing, Nothing, Just CapturedByDefense, Just OutOfBounds, Just Ingame, Just Goal])}

                        eitherDecode (encode testOffenseTeam) `shouldBe`  (Right testOffenseTeam)
                        

        describe "Genetic" $ do
            describe "Genetic.Allele"    $ do
                return ()
            describe "Genetic.Crossover" $ do
                return ()
            describe "Genetic.Mutation"  $ do
                return ()
            describe "Genetic.Selection" $ do
                return ()


testDefaultDefense :: Defense
testDefaultDefense = Defense { defActionDist = ([(Move, 50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100]) }

-- (testing purposes only)
testDefaultOffense :: Offense
testDefaultOffense = Offense { offActionDist     = ([(Move,  50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100])
                         , offBallActionDist = ([(Shoot, 50), (Dribble,   50)],                          [0, 50, 100])
                         }

