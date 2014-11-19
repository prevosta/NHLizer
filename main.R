library(XML)
source("QueryBuilder.R")

output <- vector("list", getNbViewnames()) 

for(inxViewName in 1:getNbViewnames())
{
	print(inxViewName)
	lastFirstID <- 9999
	
	for(inxPage in 1:50)
	{
		url <- getURL(inxViewName, inxPage)
		tables <- readHTMLTable(url)
		data_table <- tables[[length(tables)]]
		firstID <- as.numeric( as.character( data_table[1,1]) )
		
		if(firstID != lastFirstID)
		{
			if(is.null(output[[inxViewName]]))
			{
				output[[inxViewName]] <- data_table[,2:ncol(data_table)]
			}
			else
			{
				output[[inxViewName]] <- rbind(output[[inxViewName]], data_table[,2:ncol(data_table)]) 	
			}
			
			lastFirstID <- firstID
		}
		else
		{
			break
		}
		print('.')
	}
}

return( output )
