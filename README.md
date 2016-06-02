# hfo-genetic-server
A server written in Haskell for to communicate with Half-Field Offense for my undergraduate thesis

## Done:

  * Start and stop the HFO binary
  * Check if the simulation is still running
    * this is being done via the solo agent that connects to the server
  * Dispatch one dummy agent script

## TODO:
  * Dispatch multiple agent scripts in parallel
  * Create a python script that takes information based on cmd-flags OR designated config-files (look up YAML)
  * Create a python player that connects to this server
    * specifiy protcol
  * OR try to make it work through written txt-files
    * specify file type
    * create custom parser (yay for parsec!)
  * create repl
    * Start/Stop the simulation
    * Start with flag-options
    * Rerun simulation with seed
    * Rerun simulation based on log-files


