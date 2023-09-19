#2.Split by observer

library(dplyr)
library(data.table)

if(F)
{
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
      dt.observer <- temp %>% select(SCIENTIFIC_NAME, COUNTRY, COUNTRY_CODE, OBS_DATE, OBSERVER_ID, LOCALITY_ID)
      ls[[length(ls)+1]] <- dt.observer
    }
  }
  sp_year_month <- rbindlist(ls)
  print(paste(which(sp==a), "/", length(a)))
  sp.ls[[length(sp.ls)+1]] <- sp_year_month
}

sp.combine <- rbindlist(sp.ls)
saveRDS(sp.combine,"/Volumes/WD6T/sp_combine0918.rda")
print("Sec1 Done.")
#head(sp.combine)
}
sp.combine <- readRDS("/Volumes/WD6T/sp_combine0918.rda")
sp.combine <- split(sp.combine, sp.combine$OBSERVER_ID)
#saveRDS(sp.obsplit, "/Volumes/WD6T/sp_obsplit_list0918.rda")
print("Sec2 start.")

name <- names(sp.obsplit)
n <- name[1]
i <- 1
for(n in name)
{
  item <- sp.obsplit[[n]]
  print(paste(which(n==name), "/", length(name)))
  i <- i+1
  ob <- item$OBSERVER_ID[1]
  saveRDS(item, sprintf( "/Volumes/WD6T/by_observer_new0918/%s.rda", ob))
}


