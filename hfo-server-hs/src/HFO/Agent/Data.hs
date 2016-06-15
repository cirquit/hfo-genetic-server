module HFO.Agent.Data where

-- | All possible actions for an agent WITHOUT the possession of the ball
--
data Action  = Move           -- high level move based on strategy (whatever this might be - TODO)
             | Intercept      -- intercept the ball
             | Catch          -- goalie only  (this may be a little bit ugly)
             | NoOp           -- no operation
--             | MoveTo Int Int -- x in [-1,1], y in [-1,1]
--             | Dash Int Int   -- power in [0,100], direction in [-180,180]
--             | Turn Int       -- direction in [-180,180]
--             | Attract
    deriving (Show, Enum, Bounded)

-- | All possible actions for an agent WITH the possesion of the ball
--
data BallAction  = Shoot                -- shoot in (possibly in looking direction)
                 | Dribble              -- dribble in whatever direction?...
--                 | Pass Int             -- pass to teammate in [0,11]
--                 | Kick   Int Int       -- power in [0,100], direction in [-180, 180]
--                 | KickTo Int Int Int   -- x in [-1,1], y in [-1,1], power in [0,3]
--                 | DribbleTo Int Int    -- x in [-1,1], y in [-1,1]
    deriving (Show, Enum, Bounded)

-- | Wrapper for defense action distribution
--
--   The following should always be True:
--
--      1) foldr ((+) . snd) 0 defActionsDist == 100

data Defense = Defense { defActionDist :: [(Action, Int)] }
    deriving Show

-- | Wrapper for offense action distribution
--
--   The following should always be True:
--
--       1) foldr ((+) . snd) 0 offActionsDist     == 100
--       2) foldr ((+) . snd) 0 offBallActionsDist == 100
--
data Offense = Offense { offActionDist :: [(Action, Int)], offBallActionDist :: [(BallAction, Int)] }
    deriving Show

-- | Wrapper for Teams
--
--   The Half-Field Offense Task defines 4 offensive and 3 defensive players + goalie
--
--   These are the main genomes that will be
data OffenseTeam = OffenseTeam { op1 :: Offense, op2 :: Offense, op3 :: Offense, op4    :: Offense }
    deriving Show

data DefenseTeam = DefenseTeam { goalie :: Defense, dp2 :: Defense, dp3 :: Defense, dp4 :: Defense }
    deriving Show

-- | Defaults
--
-- (testing purposes only)
defaultDefense :: Defense
defaultDefense = Defense { defActionDist = [(Move, 50), (Intercept, 20), (Catch, 15), (NoOp, 15)]}

-- (testing purposes only)
defaultOffense :: Offense
defaultOffense = Offense { offActionDist     = [(Move,  50), (Intercept, 20), (Catch, 15), (NoOp, 15)]
                         , offBallActionDist = [(Shoot, 50), (Dribble,   50)]
                         }

-- (testing purposes only)
defaultDefenseTeam :: DefenseTeam
defaultDefenseTeam  = DefenseTeam { goalie = defaultDefense
                                  , dp2    = defaultDefense
                                  , dp3    = defaultDefense
                                  , dp4    = defaultDefense
                                  }

-- (testing purposes only)
defaultOffenseTeam ::OffenseTeam
defaultOffenseTeam = OffenseTeam { op1 = defaultOffense
                                 , op2 = defaultOffense
                                 , op3 = defaultOffense
                                 , op4 = defaultOffense
                                 }