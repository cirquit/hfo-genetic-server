{-# LANGUAGE ViewPatterns #-}

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Test
import HFO
import HFO.ToFlags


main :: IO ()
main = hspec $ do
    describe "HSpec tests for hfo-server-hs" $ do
        describe "HFO" $ do
            describe "HFO.Agent" $ do

                prop "offense agent toFlags instance" flagPropOffenseAgent
                prop "defense agent toFlags instance" flagPropDefenseAgent
                prop "offense agent actionDistribution amounts to 100"     actionDistOffenseGeneration
                prop "offense agent ballActionDistribution amounts to 100" ballActionDistOffenseGeneration
                prop "defense agent actionDistribution amounts to 100"     actionDistDefenseGeneration

            describe "HFO.Server" $ do
                it "server toFlags instance" $ do
                    let server = ServerConf
                          { showMonitor      = True
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
                    prop "Offense"     jsonPropOffense
                    prop "Defense"     jsonPropDefense
                    prop "DefenseTeam" jsonPropDefenseTeam
                    prop "OffenseTeam" jsonPropOffenseTeam
                    prop "HFOState"    jsonPropHFOState

        describe "Genetic" $ do
            describe "Genetic.Allele"    $ do
                return ()
            describe "Genetic.Crossover" $ do
                return ()
            describe "Genetic.Mutation"  $ do
                prop "offense mutations does not affect the sum rule of the distribution (sum dist = 100)" mutationOffenseDist
                prop "defense mutations does not affect the sum rule of the distribution (sum dist = 100)" mutationDefenseDist
            describe "Genetic.Selection" $ do
                return ()