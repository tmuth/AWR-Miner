# Instructions:
# 1. Place this file in the same directory as your .out files before you source them. The main R code looks for "settings.R"
#    in the working directory and sources it if it exists.
# 2. Edit the debugFilePattern variable so the regex matches ANY file in the working directory
#	 you wish to debug. 
# 3. Add debug options inside


# debug options:
#debugModeOverride <- TRUE  | rm(debugModeOverride)
#dumpCSV <- TRUE  | rm(dumpCSV)
#filePatternOverride <- "^awr-hist.+DB110g.+(\\.out|\\.gz)$" | rm(filePatternOverride)
#plotOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS] | rm(plotOverride) | plotOverride <- c("SOME","PAGE1,"AAS")
#parseOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS] | rm(parseOverride) | c("SOME","aas_facet")
#parseOverride <- c("!TOP-SQL-BY-SNAPID")
#plotOverride <- "NONE"

okToApplySettings <- FALSE

# STEP 2
# The following regex will match this file awr-hist-1796062745-PRD-96403-98825.out
debugFilePattern <- "^awr-hist.+PRD.+(\\.out|\\.gz)$"

flog.info('!!!Found settings.R file in working directory!!!',name='status')


if(exists("debugFilePattern")){
	if(!is.null(debugFilePattern)){
		if(length(list.files(pattern=debugFilePattern)) > 0){
			flog.info('!!!A file in the working dir matches the debug file pattern!!!',name='status')
			okToApplySettings <- TRUE
		}
	}
}



if(okToApplySettings==TRUE){
	# STEP 3
	# start debug options here:
	
  flog.info('!!!okToApplySettings = TRUE!!!',name='status')
	debugModeOverride <- TRUE
	dumpCSV <- TRUE 
	
  # end debug options
} else {
	flog.info('!!!okToApplySettings = FALSE!!!',name='status')
}

