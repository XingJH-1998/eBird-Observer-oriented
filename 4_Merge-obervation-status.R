#4.Merge observation data with status

library(data.table)
library(dplyr)

obs.status <- readRDS("/Volumes/WD6T/obser_nation_status0919.rda")

setwd("/Volumes/WD6T/by_observer_new")
oblist <- list.files()
ob <- oblist[1]
ls <- list()
for(ob in oblist)
{
  temp <- readRDS(ob)
  tempob1 <- temp[,.(N_COUNT=.N, N_LOCALITY=length(unique(LOCALITY_ID))), by=list(COUNTRY_CODE, OBSERVER_ID, Year, Month, SCIENTIFIC_NAME)]
  tempob1[tempob1$COUNTRY_CODE=="NA",]$COUNTRY_CODE <- "NAM"
  tempob1$obs.nation <- paste(tempob1$OBSERVER_ID, tempob1$Year, tempob1$COUNTRY_CODE)
  tempob1 <- merge(tempob1, obs.status, by="obs.nation", all.x = TRUE)
  colnames(tempob1)[9] <- "TYPE"
  ls[[length(ls)+1]] <- tempob1[,2:9]
  print(paste(which(ob==oblist),"/" ,length(oblist)))
}

ls <- rbindlist(ls)
saveRDS(ls, "/Volumes/WD6T/ebird_observer_oriented0919.rda")



