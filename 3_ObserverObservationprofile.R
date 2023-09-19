#3.Observer & Observation type profile
library(dplyr)
library(data.table)

setwd("/Volumes/WD6T/by_observer_new0913")
oblist <- list.files()
ob <- oblist[1]
ls <- list()
for(ob in oblist)
{
  temp <- readRDS(ob)
  temp <- temp %>% mutate(Year=year(OBS_DATE), Month=month(OBS_DATE))
  temp.se <- temp[,c(2,3,5:7)] %>% unique()
  ls[[length(ls)+1]] <- temp.se
  print(which(ob==oblist))
}
obser <- rbindlist(ls)
###Namibia abbr. NA
obser[which(obser$COUNTRY=="Namibia"),]$COUNTRY_CODE <- "NAM"

#Observer status
Schengen<-c("DE", "AT", "BE", "CZ", "DK", 
            "EE", "FI", "FR", "GR", "HU", "IS", "IT",
            "LV", "LI", "LT", "LU", "MT", 
            "NL", "NO", "PL", "PT", "SK", 
            "SI", "ES", "SE", "CH", "GB", "HR") 
Schengen <- data.frame(Schengen)
Schengen$eu <- "EU"

obser[obser$COUNTRY_CODE=="NA",]$COUNTRY_CODE <- "NAM"
obser$Country <- obser$COUNTRY_CODE
obser$Country[obser$COUNTRY_CODE %in% Schengen$Schengen] <- "EU"
#obser <- obser %>% mutate(Year=year(OBS_DATE), Month=month(OBS_DATE))
obser.se <- obser[,.(N.country=1), by=list(OBSERVER_ID, Year,Country)]
obser.se.se <- obser.se[,.(N.country=.N), by=list(OBSERVER_ID, Year)]
obser.se.se$observer <- paste(obser.se.se$OBSERVER_ID, obser.se.se$Year)
obser.se.se$status <- obser.se.se$N.country
obser.se.se$status[obser.se.se$N.country>1] <- "International"
obser.se.se$status[obser.se.se$N.country<=1] <- "Local"
obser.status <- obser.se.se[,4:5]

saveRDS(obser.status, "/Volumes/WD6T/obser_status0918.rda")
#obser.status <- readRDS( "/Volumes/WD6T/obser_status0915.rda")

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

obser.withstatus <- merge(obser, obser.se.se[,c(1, 2, 5)], by=c("OBSERVER_ID", "Year"))
obser.withstatus$observer <- paste(obser.withstatus$OBSERVER_ID, obser.withstatus$Year)
ob.intern <- obser.withstatus %>% filter(status=="International")
ob <- ob.intern$observer %>% unique()
ob1 <- ob[1]
ls.nation <- list()
for(ob1 in ob)
{
  temp.ob <- subset(ob.intern, observer==ob1)
  temp.nation <- as.data.frame(table(temp.ob$Country))
  nation <- nationality(temp.nation)
  nation$observer <- ob1
  ls.nation[[length(ls.nation)+1]] <- nation
  print(which(ob1==ob))
}
observer.nation <- rbindlist(ls.nation)
observer.nation <- observer.nation %>% mutate(Nationality=ifelse(ratio1>=.5, Nation1, "International"))
observer.nation$obs.nation <- paste(observer.nation$observer, observer.nation$Nationality)
obser <- obser %>% mutate(observer = paste(OBSERVER_ID, Year), obs.nation= paste(OBSERVER_ID, Year, Country))
obser <- merge(obser,obser.status, by="observer")
obser <- merge(obser, observer.nation[,c(5,6)], by="observer", all.x=TRUE)
obser[obser$status=="Local",]$Nationality <- obser[obser$status=="Local",]$Country
obser <- obser %>% mutate(obs.nation.status = ifelse(status=="Local", "Local", ifelse(Nationality==Country, "Local", "International")))
obser.nation.status <- obser[,c(1,3:11)]
obser.nation.status$obs.nation <- paste(obser.nation.status$observer, obser.nation.status$COUNTRY_CODE)
obser.nation.status <- obser.nation.status[,c(10,11)] %>% unique()
saveRDS(obser.nation.status, "/Volumes/WD6T/obser_nation_status0919.rda")
saveRDS(obser,"/Volumes/WD6T/obser_nation_status0919_detailed.rda" )








