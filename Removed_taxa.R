library(data.table)
taxa<-readRDS("../eBird202212/taxa_number_202212.rda")
item1<-taxa[grepl("[^A-Za-z[:blank:]]", SCIENTIFIC_NAME)]
item2<-taxa[grepl(" x ", SCIENTIFIC_NAME)]

removed_taxa<-rbindlist(list(item1, item2))
left_taxa<-taxa[!(SCIENTIFIC_NAME %in% removed_taxa$SCIENTIFIC_NAME)]
saveRDS(unique(removed_taxa), "../eBird202212/removed_taxa_202212.rda")
splist <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/removed_taxa_202212.rda")
write.csv(splist[,c(1,2)], "/Users/tangerine/Documents/Lab_data/ebird_covid/removed_taxa.csv")
