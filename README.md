# hfo-genetic-server
A server written in Haskell for to communicate with Half-Field Offense for my undergraduate thesis

## Done:

  * Start and stop the HFO binary
  * Check if the simulation is still running
    * this is being done via the solo agent that connects to the server
  * Dispatch one dummy agent script

  * Dispatch multiple agent scripts in parallel
  * look up every possivle action for defender / attacker and try to run them in the python script from a cmd given distribution 
  * Create a python script that takes information based on cmd-flags OR designated config-files (look up YAML)
  * Automatic property tests for mutation, generation of offense/defense individuals, (de)/serializing JSON of every data type (in Haskell)
  * create json parser in python for my data type
  * Rewrite the python + haskell code to start the server only ONCE for all genomes and create a serializable format for the genomes which Haskell and Python can parse and update with ease in JSON (in progress)
  * create repl-like-functions
    * Start/Stop the simulation
    * get automatic calculations
      * fitness
  * created segmentation of the field in 16 subfields so every player can learn a tactic corresponding to their current position

## TODO:
  * Start with simple goals
    * 2 offensive players vs base
    * 2 defensive players vs base
    * 2 off vs 2 def and test vs base
      * plays vs random teams
  * think about a "shout" functionality where a player can call out a pass to them
  * think about representation of associations between agents
  * create repl-like-functions
    * calculate everything needed for gnuplot
    * Rerun simulation with seed
    * Rerun simulation based on log-files (this won't work because every logs after the first run are broken (?))
  * Create examples for every encoding possible on LOW/HIGH Feature space. It's easier to understand and run examples than read a pdf

## Possible ways to go forward
  * split the field in 4 parts and learn P(Action)
      ** with new/old Mutation and Crossover
      ** goalOpening or xPos as fitness
  * new parametrized actions
      ** Shoot if the goalAngle (1) is big enough, else pass to a teammate (2)
      ** Dribble to teammate (1)
      ** Break clear without ball vs two players in direction calculated by the orthogonal vector from the connection between the opponents
      ** Pass to teammate (1)
      ** Intercept based on distance to ball (1)
  * players have passive properties, actions are partially dependent on these
      ** Aggressivity
          *** affects the strength of Shoot/Pass
          *** affects the distance where the player tries to intercept the ball
      ** Teamwork
          *** breaking clear (unknown parameter)
          *** affects smart shoot/pass

  * Actionchains instead of actionpicking every frame, create a FIFO stack, this allows for more complex actions like break free
     ** parameters of these high-level actions
        *** count of atomar actions (dependend on global state or passive properties)
        *** which state is it dependend on
     ** need to create multiple single-step actions, like
        *** move away from enemy
        *** pass to nearest / best (dependend on angle) teammate
        *** turn to ball
     ** break free example
        *** [moveawayfromenemy, moveawayfromenemy, moveawayfromenemy, turntoball]
        *** every subaction is dependend on the global state

    * Simple NN for generation of the policy
    * or RL with whole state -> policy table

## Info for the future:

  * If the server runs in '-no-sync' mode (~ not a realtime simulation), one can not check only one of the python agents for an exitcode to notice if the simulation is done. This behaviour is undefined und random. One solution to this would be to check every agent script and if anyone terminates, then the simulation should be over (this problem does not occur without -no-synch)
  * One can not connect the players to the server without a delay (currently 1s after every player), so it would be better to start the server only once for all simulations
  * This delay is dependent of the workload on your machine...without -no-synch and without a monitor it needs to be >600ms. without -no-synch 500-600ms...in conclusion it's a very bad idea to connect more than once
  * txt-file based data exchange is not a very good idea because of lazy io in Haskell (text-package fixed it nonetheless)
  * After ~24200 +/-250 steps without restart the server starts to behave strange und kicks sometimes the players before they played enough episodes. Happens for me if generation = 50 and episodes = 20 so we get 1000 games. I have to check in my 'startSimulation' and restart accordingly if the last individual was not evaluated
  * giveBallToPlayer does not work. It gives the ball to a random player - nope, starts the enumeration from 1-11, even if the first player has the number 7
  * my robust variance algorithm requires two passes for similar long floating point lists (E[X^2] - E[X]^2 does not work, even with a location parameter (E[(X-k)^2] - E[X-k]^2))