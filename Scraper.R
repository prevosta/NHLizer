scrapePlayers <- function(scheme, host, path, years = 1998:2015, pages = 1:50, viewnames = c("summary",
											     "assists",
											     "bios",
											     "divisionScoring",
											     "faceOffPercentage",
											     "faceOffPercentageAll",
											     "goals",
											     "homeScoring",
											     "penalties",
											     "plusMinus",
											     "points",
											     "rtssPlayerStats",
											     "roadScoring",
											     "shooting",
											     "shootingAll",
											     "shootouts",
											     "scoringLeaders",
											     "timeOnIce") )
{
	values <- c("20152ALLSASALL", "", "player.bioFirstNameLastName", "")
	names(values) <- c("fetchKey", "viewName", "sort", "pg")
	
	output <- vector("list", (length(viewnames) * length(years) ) )
	
	for(inxYear in years)
	{
		for(inxViewName in 1:length(viewnames))
		{
			cat(sprintf("\n %d - %s \n", inxYear, viewnames[inxViewName]))
			
			lastFirstName <- ""
			inxOutput <- ((inxYear-min(years))*length(viewnames)) + inxViewName
			
			for(inxPage in pages)
			{
				values["fetchKey"] = paste(as.character(inxYear), "2ALLSASALL", sep = "")
				values["viewName"] = as.character(viewnames[inxViewName])
				values["pg"] = as.character(inxPage)
				
				query <- getQuery(values)
				url <- getURL(scheme, host, path, query)
				
				output[[inxOutput]] <- scrape(url, output[[inxOutput]])
				
				cat(".")
			}
		}
	}
	
	output
}

scrapeTeams <- function(scheme, host, path, years = 1998:2015, pages = 1:50, viewnames = c("summary",
											   "goalsHomeRoad",
											   "goalsByPeriod",
											   "goalsFor",
											   "goalsAgainst",
											   "goalGames",
											   "outshootingOutshotby",
											   "overtimeRecords",
											   "penalties",
											   "powerPlay",
											   "powerPlayTime",
											   "penaltyKill",
											   "penaltyKillTime",
											   "plusMinusTeam",
											   "realTimeStats",
											   "recordWhenLeading",
											   "scoreTrailFirst",
											   "shootoutTeamSummary") )
{
	values <- c("20152ALLSAAAll", "", "team.teamName", "")
	names(values) <- c("fetchKey", "viewName", "sort", "pg")
	
	output <- vector("list", (length(viewnames) * length(years) ) )
	
	for(inxYear in years)
	{
		for(inxViewName in 1:length(viewnames))
		{
			cat(sprintf("\n %d - %s \n", inxYear, viewnames[inxViewName]))
			
			lastFirstName <- ""
			inxOutput <- ((inxYear-min(years))*length(viewnames)) + inxViewName
			
			for(inxPage in pages)
			{
				values["fetchKey"] = paste(as.character(inxYear), "2ALLSAAAll", sep = "")
				values["viewName"] = as.character(viewnames[inxViewName])
				values["pg"] = as.character(inxPage)
				
				query <- getQuery(values)
				url <- getURL(scheme, host, path, query)
				
				output[[inxOutput]] <- scrape(url, output[[inxOutput]])
				
				cat(".")
			}
		}
	}
	
	output
}

scrapeGames <- function(scheme, host, path, years = 1998:2015, pages = 1:50, viewnames = c("summary",
											   "gameSummary",
											   "gameByGameOvertimesAndShootouts",
											   "teamPenaltiesByGame",
											   "penaltyShots",
											   "teamRTSSreports",
											   "gameByGameShutouts") )
{
	values <- c("20152ALLSATAll", "", "team.teamName", "")
	names(values) <- c("fetchKey", "viewName", "sort", "pg")
	
	output <- vector("list", (length(viewnames) * length(years) ) )
	
	for(inxYear in years)
	{
		for(inxViewName in 1:length(viewnames))
		{
			cat(sprintf("\n %d - %s \n", inxYear, viewnames[inxViewName]))
			
			lastFirstName <- ""
			inxOutput <- ((inxYear-min(years))*length(viewnames)) + inxViewName
			
			for(inxPage in pages)
			{
				values["fetchKey"] = paste(as.character(inxYear), "2ALLSATAll", sep = "")
				values["viewName"] = as.character(viewnames[inxViewName])
				values["pg"] = as.character(inxPage)
				
				query <- getQuery(values)
				url <- getURL(scheme, host, path, query)
				
				output[[inxOutput]] <- scrape(url, output[[inxOutput]])
				
				cat(".")
			}
		}
	}
	
	output
}

scrape <- function(url, output)
{
	html_tables <- readHTMLTable(url)
	data_table <- html_tables[[length(html_tables)]]
	firstName <- as.character( paste(data_table[1,2], data_table[1,2]) )
	lastFirstName <- ""
	
	if(firstName != lastFirstName)
	{
		if(is.null(output))
		{
			output <- data_table[,2:ncol(data_table)]
		}
		else
		{
			output <- rbind(output, data_table[,2:ncol(data_table)]) 	
		}
		
		lastFirstName <- firstName
	}
	else
	{
		break
	}
	
	return (output)
}

getQuery <- function(values)
{	
	query <- "?"
	
	for( inx in 1:length(values) )
	{
		query <- paste(query, names(values)[inx], "=", as.character(values[inx]), "&",  sep = "")
	}
	
	return( substr(query, 1, nchar(query)-1) )
}

getURL <- function(scheme, host, path, query)
{
	return( paste( scheme, "://", host, "/", path, query, sep = "") )
}