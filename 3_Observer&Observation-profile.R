#3.Observer & Observation type profile
library(dplyr)
library(data.table)


#Observer status
Schengen<-c("DE", "AT", "BE", "CZ", "DK", 
            "EE", "FI", "FR", "GR", "HU", "IS", "IT",
            "LV", "LI", "LT", "LU", "MT", 
            "NL", "NO", "PL", "PT", "SK", 
            "SI", "ES", "SE", "CH", "GB", "HR") 
Schengen <- data.frame(Schengen)
Schengen$eu <- "EU"

setwd("/Volumes/WD6T/by_observer_new")
oblist <- list.files()
ob <- oblist[100040]
ls <- list()
for(ob in oblist)
{
  obser <- readRDS(ob)
  obser$Country <- obser$COUNTRY_CODE
  obser$Country[obser$COUNTRY_CODE %in% Schengen$Schengen] <- "EU"
  obser.se <- obser[,.(N.country=1), by=list(OBSERVER_ID, Year,Country)]
  obser.se <- na.omit(obser.se)
  obser.se.se <- obser.se[,.(N.country=.N), by=list(OBSERVER_ID, Year)]
  obser.se.se$observer <- paste(obser.se.se$OBSERVER_ID, obser.se.se$Year)
  obser.se.se$status <- obser.se.se$N.country
  obser.se.se$status[obser.se.se$N.country>1] <- "International"
  obser.se.se$status[obser.se.se$N.country<=1] <- "Local"
  obser.status <- obser.se.se[,4:5]
  ls[[length(ls)+1]] <- obser.status
}
obser.status <- rbindlist(ls)
saveRDS(obser.status, "/Users/tangerine/Documents/Lab_data/ebird_covid/obser_status.rda")

nationality <- function(temp.nation)
{
  freq.sum <- sum(temp.nation$Freq)
  max1.nation <- as.character(temp.nation[which.max(temp.nation$Freq),]$Var1)
  ratio1 <- max(temp.nation$Freq)/freq.sum
  temp.nation <- temp.nation[-which.max(temp.nation$Freq),]
  max2.nation <- as.character(temp.nation[which.max(temp.nation$Freq),]$Var1)
  ratio2 <- max(temp.nation$Freq)/freq.sum
  nation <- data.frame(Nation1=max1.nation, ratio1=ratio1, Nation2=max2.nation, ratio2= ratio2)
  return(nation)
}

ls <- list()
obser.status <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/obser_status.rda")
for(ob in oblist)
{
  obser <- readRDS(ob)
  obser <- obser %>% select(COUNTRY_CODE, OBSERVER_ID, Year, Month) %>% unique()
  obser$Country <- obser$COUNTRY_CODE
  obser$Country[obser$COUNTRY_CODE %in% Schengen$Schengen] <- "EU"
  obser$observer <- paste(obser$OBSERVER_ID, obser$Year)
  obser <- merge(obser, obser.status, by="observer", all.x=TRUE)
  ls[[length(ls)+1]] <- obser
}
obser <- rbindlist(ls)

ls <- list()
ob.intern <- obser %>% filter(status=="International")
oblist <- unique(ob.intern$observer)
ob <- oblist[1]
for(ob in oblist)
{
  temp.ob <- subset(ob.intern, observer==ob)
  temp.nation <- as.data.frame(table(temp.ob$Country))
  nation <- nationality(temp.nation)
  nation$observer <- ob
  ls[[length(ls)+1]] <- nation
  print(which(ob==oblist))
}
observer.nation <- rbindlist(ls)
saveRDS(observer.nation, "observer_nation.rda")

observer.nation <- readRDS("observer_nation.rda")
obser$obs.nation <- paste(obser$observer, obser$Country)
obser1 <- obser %>% filter(status=="Local")
obser2 <- obser %>% filter(status=="International")
observer.nation <- observer.nation %>% filter( ratio1 >.5)
observer.nation$ obs.nation <- paste(observer.nation$observer, observer.nation$Nation1)
observer.nation$ Nation.status <- "Local"
obser2 <- merge(obser2, observer.nation[,c(6,7)], by="obs.nation", all.x = TRUE)
obser2$Nation.status[is.na(obser2$Nation.status)] <- "International"
obser1$Nation.status <- obser1$status
obser.nation.status <- rbind(obser1,obser2)
saveRDS(obser.nation.status, "/Users/tangerine/Documents/Lab_data/ebird_covid/obser_nation_status.rda")





