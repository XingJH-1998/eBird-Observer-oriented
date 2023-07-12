#4.Merge observation data with status

library(data.table)
library(dplyr)

obs.status <- readRDS("/Volumes/WD6T/obser_nation_status.rda")
obs.status$obs.nation <- paste(obs.status$OBSERVER_ID, obs.status$Year, obs.status$COUNTRY_CODE)
obs.status <- obs.status[,c(11,12)] %>% unique() ###check

setwd("/Volumes/WD6T/by_observer_new")
oblist <- list.files()
ob <- oblist[1]
ls <- list()
for(ob in oblist)
{
  temp <- readRDS(ob)
  tempob1 <- temp[,.(N_COUNT=.N, N_LOCALITY=length(unique(LOCALITY_ID))), by=list(COUNTRY_CODE, OBSERVER_ID, Year, Month, SCIENTIFIC_NAME)]
  tempob1$obs.nation <- paste(tempob1$OBSERVER_ID, tempob1$Year, tempob1$COUNTRY_CODE)
  tempob1 <- merge(tempob1, obs.status, by="obs.nation", all.x = TRUE)
  ls[[length(ls)+1]] <- tempob1[,2:7]
  print(which(ob==oblist))
}

ls <- rbindlist(ls)
saveRDS(ls, "/Volumes/WD6T/species_observer_country_year_month_new.rda")



