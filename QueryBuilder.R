
viewnames = c("summary",
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
	      "timeOnIce")

query = c("20152ALLSASALL", "", "player.bioFirstNameLastName", "")
names(query) <- c("fetchKey", "viewName", "sort", "pg")

scheme <- "http://"
host <- "www.nhl.com"
path <- "/ice/playerstats.htm"

getQuery <- function(inxViewName, inxPage)
{
	query["viewName"] = as.character(viewnames[inxViewName])
	query["pg"] = as.character(inxPage)
	
	q <- "?"
	for( inx in 1:length(query) )
	{
		q <- paste(q, names(query)[inx], "=", as.character(query[inx]), "&",  sep = "")
	}
	return( substr(q, 1, nchar(q)-1) )
}

getURL <- function(inxViewName, inxPage)
{
	return( paste( scheme, host, path, getQuery(inxViewName, inxPage), sep = "") )
}

getNbViewnames <- function()
{
	return( length(viewnames) )
}