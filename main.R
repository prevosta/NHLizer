library(XML)
source("Scraper.R")

scheme <- "http"
host <- "www.nhl.com"
path <- "ice/playerstats.htm"

players_Data <- scrapePlayers(scheme, host, path, years = 2015, page = 1:2)
teams_Data <- scrapeTeams(scheme, host, path, years = 2015, page = 1:2)