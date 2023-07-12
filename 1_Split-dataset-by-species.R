#1.Split eBird dataset by species

library(data.table)
setwd("/media/huijieqiao/WD10T/eBird_covid19/eBird_covid19")
base<-"/media/huijieqiao/WD10T/datasets_harvest/eBird202212/"
sp_records<-readRDS(sprintf("%s/taxa_number_202212.rda", base))
sp_records$good_name<-T

#Filter unclear species
sp_records[grepl("/", SCIENTIFIC_NAME)]$good_name<-F
sp_records[grepl("sp\\.", SCIENTIFIC_NAME)]$good_name<-F
sp_records[grepl(" x ", SCIENTIFIC_NAME)]$good_name<-F
sp_records[grepl("\\[", SCIENTIFIC_NAME)]$good_name<-F
sp_records[grepl("\\(", SCIENTIFIC_NAME)]$good_name<-F
table(sp_records$good_name)

y_m<-data.table(expand.grid(year=c(2018:2022), month=c(1:12)))
i<-1
j<-1
good_sp_records<-sp_records[good_name==T]
#good_sp_records<-good_sp_records[sample(nrow(good_sp_records), nrow(good_sp_records))]
setorderv(good_sp_records, "N_Record", -1)
for (i in c(1:nrow(good_sp_records))){
  print(paste(i, nrow(good_sp_records), good_sp_records[i]$SCIENTIFIC_NAME, good_sp_records[i]$N_Record))
  target<-sprintf("%s/by_observer/%s.rda", base, good_sp_records[i]$SCIENTIFIC_NAME)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  df_list<-list()
  for (j in c(1:nrow(y_m))){
    rda_f<-sprintf("%s/by_species/%s/%d/%d.rda", base, good_sp_records[i]$SCIENTIFIC_NAME, y_m[j]$year, y_m[j]$month)
    if (file.exists(rda_f)){
      df<-readRDS(rda_f)
      df_observer<-df[, .(N=.N, N_LOCALITY=length(unique(LOCALITY))), by=list(COUNTRY_CODE, OBSERVER_ID, Year, Month)]
      df_observer$SCIENTIFIC_NAME<-good_sp_records[i]$SCIENTIFIC_NAME
      df_list[[length(df_list)+1]]<-df_observer
    }
  }
  df_list<-rbindlist(df_list)
  saveRDS(df_list, target)
}









