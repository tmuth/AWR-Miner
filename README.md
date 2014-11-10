AWR-Miner
=========

A tool to mine and graph the AWR Repository of an Oracle 10.1+ database for performance data 

# Basic Instructions
- Install R 3.x+
- Install RStudio
- Open R-AWR-Mining.R file in RStudio
- Set the working directory to the location of the .out or .out.gz files in RStudio via the "Session" menu > "Set Working Directory". You could also use ctrl+shift+H
- "Source" the R-AWR-Mining.R in RStudio via the Source button in the top+right or press ctrl+shift+s
- AWR-Miner will plot all files in this working directory

# Options
Options can be set interactively in an IDE like RStudio in the "console" section or through a file named very specifically *settings.R*. Using settings.R is the more reliable and preferred method. 

The *settings.R* file should be placed in the same directory as the .out files. This allows you to have a different settings.R file for each directory of files.

The following options can be added to settings.R

## File Pattern Override
`filePatternOverride <- "^awr-hist.+FOO.+(\\.out|\\.gz)$"`
The example above would only look for .out or .gz files with a name that contains "FOO". This an easy way to limit parsing to just a file or subset of files without having to move files into new directories. The default file patterh is:
`"^awr-hist*.*(\\.out|\\.gz)$"`


## Debug Mode
`debugModeOverride <- TRUE`
`rm(debugModeOverride)`
Debug mode is useful for a number of situations, including:
- dumping parsed data out to .Rda format files
- errors encountered with a particular file
- timing of specific components

In debug mode, the parsed data for each database is saved as highly compressed .Rda files. These are also used for the "combined stats" scripts.

The following are sub-options of debug mode, ie you must be in debug mode first for these to work.
### Debug Mode > dumpCSV
`dumpCSV <- TRUE`
This also dumps all data frames to comma separated files.

### Debug Mode > debugMoveFiles
`debugMoveFiles <- TRUE`
This option moves all completed .out files into a *done* subdirectory. If you have a lot of files, but only a few have errors, this allows you to separate the valid files from the ones that have errors. 

*End of Debug Mode Options*

## Parse Override
```
parseOverride <- c("!TOP-SQL-BY-SNAPID") # default, don't parse TOP-SQL-BY-SNAPID
parseOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS]
parseOverride <- c("SOME","aas_facet")
rm(parseOverride)
```
This is useful if there is a section with errors that won't parse. Also if you just want to extract certain data. It can cause errors, so if just extracting data to .Rda or .csv, `set plotOverride <- "NONE"`


## Plot Override
```
plotOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS] | 
plotOverride <- "NONE" # Don't plot anything, often combined with parse override and debug mode to dump data
plotOverride <- c("SOME","PAGE1,"AAS")
rm(plotOverride)
```
This allows you to control what is output to PDF. It's useful for debugging. 
