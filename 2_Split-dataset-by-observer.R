#2.Split by observer

library(dplyr)
library(data.table)

setwd("/Volumes/WD6T/by_species")
a <- list.files()
sp <- a[1]
sp.ls <- list()

###read in each species' monthly data
for(sp in a)
{
  ls <- list()
  year <- list.files(sprintf("/Volumes/WD6T/by_species/%s",sp))
  year <- year[year>=2018]
  if(length(year)==0){next;}
  ye <- year[1]
  for(ye in year)
  {
    month <- list.files(sprintf("/Volumes/WD6T/by_species/%s/%s", sp, ye))
    mo <- month[1]
    for(mo in month)
    {
      temp <- readRDS(sprintf("/Volumes/WD6T/by_species/%s/%s/%s", sp, ye, mo))
      dt.observer <- temp %>% select(SCIENTIFIC_NAME, COUNTRY_CODE, LOCALITY_ID, LATITUDE, LONGITUDE, Year, Month, OBSERVER_ID)
      ls[[length(ls)+1]] <- dt.observer
    }
  }
  sp_year_month <- rbindlist(ls)
  print(which(sp==a))
  sp.ls[[length(sp.ls)+1]] <- sp_year_month
}

sp.combine <- rbindlist(sp.ls)
#saveRDS(sp.combine,"/Volumes/WD6T/sp_combine.rda")

head(sp.combine)
sp.obsplit <- split(sp.combine, sp.combine$OBSERVER_ID)
#saveRDS(sp.obsplit, "/Volumes/WD6T/sp_obsplit_list.rda")

names(sp.obsplit)
i <- 1
for(n in names(sp.obsplit))
{
  item <- sp.obsplit[[n]]
  print(paste(i, nrow(item)))
  i <- i+1
  ob <- item$OBSERVER_ID[1]
  saveRDS(item, sprintf( "/Volumes/WD6T/by_observer_new/%s.rda", ob))
}


