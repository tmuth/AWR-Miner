AWR-Miner
=========

A tool to mine and graph the AWR Repository of an Oracle 10.1+ database for performance data 

# Options
Options can be set interactively in an IDE like RStudio in the "console" section or through a file named very specifically *settings.R*. Using settings.R is the more reliable and preferred method. 

The following options can be added to settings.R

## Debug Mode
`debugModeOverride <- TRUE`
Debug mode is useful for a number of situations, including:
- dumping parsed data out to .Rda format files
- errors encountered with a particular file
- timing of specific components

In debug mode, the parsed data for each database is saved as highly compressed .Rda files. These are also used for the "combined stats" scripts.

The following are 
