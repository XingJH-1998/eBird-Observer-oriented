### Figures and data used
library(data.table)
library(dplyr)
library(ggplot2)

##Data
setwd("/Volumes/WD6T/by_observer_new")
a <- list.files()
a1 <- a[1]
ob.month.ls <- list()
ob.year.ls <- list()
ob.ls <- list()
ob.locality.number.ls <- list()
observation.locality.ls <- list()
species.locality.year.ls <- list()
for(a1 in a)
{
  item <- readRDS(a1)
  ob.month <- item[,.(N=.N, N_species=length(unique(SCIENTIFIC_NAME)), N_locality=length(unique(LOCALITY_ID))), by=list(Year, Month, OBSERVER_ID)]
  ob.month.ls[[length(ob.month.ls)+1]] <- ob.month
  
  ob.year <- item[,.(N=.N, N_species=length(unique(SCIENTIFIC_NAME)), N_locality=length(unique(LOCALITY_ID))), by=list(Year, OBSERVER_ID)]
  ob.year.ls[[length(ob.year.ls)+1]] <- ob.year
  
  ob <- item[,.(N=.N, N_species=length(unique(SCIENTIFIC_NAME)), N_locality=length(unique(LOCALITY_ID))), by=list(OBSERVER_ID)]
  ob.ls[[length(ob.ls)+1]] <- ob
  
  ob.locality.number <- item %>% select(LONGITUDE, LATITUDE, OBSERVER_ID, Year) %>% unique()
  ob.locality.number$LONGITUDE <- floor(ob.locality.number$LONGITUDE)
  ob.locality.number$LATITUDE <- floor(ob.locality.number$LATITUDE)
  ob.locality.number.ls[[length(ob.locality.number.ls)+1]] <- ob.locality.number
  
  observation.locality <- item[,.(N_observation=.N), by=list(LONGITUDE, LATITUDE, Year)] %>% floor()
  observation.locality.ls[[length(observation.locality)+1]] <- observation.locality
  
  species.locality.year <- item %>% select(LONGITUDE, LATITUDE, Year, SCIENTIFIC_NAME) %>% unique() 
  species.locality.year$LONGITUDE <- floor(species.locality.year$LONGITUDE)
  species.locality.year$LATITUDE <- floor(species.locality.year$LATITUDE)
  species.locality.year.ls[[length(species.locality.year.ls)+1]] <- species.locality.year
  print(which(a1==a))
}

ob.month <- rbindlist(ob.month.ls) #每个人每个月在多少个地点看了多少鸟
ob.year <- rbindlist(ob.year.ls) #每个人每年在多少个地点看了多少鸟
ob <- rbindlist(ob.ls) #每个人在多少个地点看了多少鸟
ob.locality.number <- rbindlist(ob.locality.number.ls) #每个locality格子里每年有多少个人去看过鸟
observation.locality <- rbindlist(observation.locality.ls) #每个locality格子里每年有多少条观测
species.locality.year <- rbindlist(species.locality.year.ls) #每个locality格子里每年有多少种鸟

saveRDS(ob.month, "/Volumes/WD6T/ob_month.rda")
saveRDS(ob.year, "/Volumes/WD6T/ob_year.rda")
saveRDS(ob, "/Volumes/WD6T/ob.rda")
saveRDS(ob.locality.number, "/Volumes/WD6T/ob_locality_number.rda")
saveRDS(observation.locality, "/Volumes/WD6T/observation_locality.rda")
saveRDS(species.locality.year, "/Volumes/WD6T/species_locality_year.rda")

###Visualization
ob <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob.rda")
ob.year <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_year.rda")
ob.month <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_month.rda")

p1 <- ggplot(ob.year %>% filter(N_species>=10), aes(x=log(N_species)))+
  geom_histogram(aes(y=stat(density)), fill="indianred")+
  geom_density(aes(y=stat(density)), linewidth=.5)+
  facet_wrap(~Year, ncol = 5)+
  ylab("Frequency")+
  labs(title="(a) Number of species observered (log-transformed)")+
  xlab(NULL)+
  theme_bw()

p2 <- ggplot(ob.year %>% filter(N_locality>=10), aes(x=log(N_locality)))+
  geom_histogram( aes(y=stat(density)), fill="steelblue")+
  geom_density(aes(y=stat(density)), linewidth=.5)+
  facet_wrap(~Year, ncol=5)+
  ylab("Frequency")+
  labs(title="(b) Number of localities (log-transformed)")+
  xlab(NULL)+
  theme_bw()

p.observer <- ggpubr::ggarrange(p1, p2, ncol=1)
ggsave("Fig1.png")


obser.status <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/obser_nation_status.rda")
status.se <- obser.status[,.(N=.N), by=list(Year, Month, Nation.status)]
status.se$Month <- month.name[status.se$Month]
status.se$date <- paste0(status.se$Year,"-",status.se$Month,"-01")
Sys.setlocale("LC_TIME","C")
status.se$date <- as.Date(status.se$date, format = "%Y-%B-%d")
status.re <- status.se %>% tidyr::spread(key = Nation.status, value = N)

ggplot(status.re, aes(x=date))+
  geom_line(aes(y=Local, color="Local"))+
  geom_line(aes(y=International*7, color="International"))+
  scale_y_continuous(name = "Number of Local Observer", sec.axis = sec_axis(~./7, name = "Number of International Observer"))+
  xlab("Date")+
  theme_bw()+
  theme(axis.title.y.right =element_text(angle=90))+
  scale_color_manual(values = c("Local"="steelblue", "International"="indianred"))+
  labs(color="Observer type")
ggsave("Fig2.png")


data.se <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/species_status_sum_month.rda")
data.se.se <- data.se[,.(N_sp=.N), by=list(Year, Month, Nation.status)]
data.se.se <- merge(data.se.se, data.se.se00, by=c("Year","Month"))
data.se.se$date <- as.Date(paste0(data.se.se$Year,"-",data.se.se$Month,"-01"))

ggplot(data.se.se, aes(x=date, fill=Nation.status))+
  geom_bar(aes(y=N_sp), position = "dodge", stat="identity")+
  theme_bw()+
  ylab("Number of species recorded")+
  geom_line(aes(y=N_spsum, color="Total Species Number"),linetype="dashed", size=.8)+
  scale_color_manual(values = "gray", guide=guide_legend(title = ""))+
  coord_cartesian(ylim = c(3000,9500))+
  xlab("Date")+
  scale_fill_manual(values = c("Local"="steelblue", "International"="indianred"))
ggsave("Fig3.png")


data <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/species_country_new_status_sum.rda")
table(data$COUNTRY)
length(unique(data$COUNTRY))
data.1 <- data
data.1.se <- data.1[,.(Nsum=sum(N.sum)), by=list(COUNTRY, Nation.status,Year)]

library(countrycode)

data.1.se$continent <- countrycode(data.1.se$COUNTRY,"country.name","continent")

data.1.se[which(data.1.se$continent=="Asia")]$continent <- countrycode(data.1.se[which(data.1.se$continent=="Asia")]$COUNTRY,"country.name","un.regionsub.name")
table(data.1.se$continent)
data.1.se[which(data.1.se$continent=="Europe")]$continent <- countrycode(data.1.se[which(data.1.se$continent=="Europe")]$COUNTRY,"country.name","un.regionsub.name")

p1 <- ggplot(data.1.se %>% filter(continent=="South-eastern Asia"))+
  geom_bar(stat = "identity", position = "fill", aes(y=COUNTRY, x=Nsum, fill=Nation.status))+
  facet_wrap(~Year, ncol = 5)+
  ggtitle("(a) South-eastern Asia")+
  xlab("Percentage")+
  ylab(NULL)+
  labs(fill="Observation type")+
  theme_bw()+
  scale_fill_manual(values =c( "Local"="steelblue", "International"="indianred"))

p2 <- ggplot(data.1.se %>% filter(continent=="Western Europe") %>% filter(COUNTRY!="Monaco"))+ ###Monaca only have international observations, which is an outlier
  geom_bar(stat = "identity", position = "fill", aes(y=COUNTRY, x=Nsum, fill=Nation.status))+
  facet_wrap(~Year, ncol = 5)+
  ggtitle("(b) Western Europe")+
  ylab(NULL)+
  xlab("Percentage")+
  labs(fill="Observation type")+
  theme_bw()+
  scale_fill_manual(values =c( "Local"="steelblue", "International"="indianred"))

p.combine <- ggpubr::ggarrange(p1, p2, nrow = 2, ncol = 1, common.legend = TRUE, legend="right")
ggsave("Fig4.png")


data.se$date <- as.Date(paste0(data.se$Year, "-", data.se$Month, "-01"))
data.se.fig4 <- data.se[,.(Sum.sp=sum(Sum)), by=list(Year, Month, Nation.status, Threat)]
data.se.fig4$date <- as.Date(paste0(data.se.fig4$Year,"-", data.se.fig4$Month,"-01"))
data.se.fig4.2 <- tidyr::spread(data.se.fig4, key=Nation.status, value = Sum.sp)
data.se.fig4.2$ratio <- data.se.fig4.2$International/data.se.fig4.2$Local
ggplot(data.se.fig4.2, aes(x=date))+
  geom_line(aes(y=ratio, group=Threat, color=Threat))+
  theme_bw()+
  xlab("Date")+
  ylab("Percentage of International Observation")+
  labs(color="Species status")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("no"="steelblue", "Threatened"="indianred"), labels = c("no"="Not Threatened", "Threatened"="Threatened"))
ggsave("Fig5.png")



library(raster)
library(rgdal)
library(sf)
library(terra)

setwd("/Volumes/WD6T/Shape")
a <- list.files()
a <- sample(a, size = length(a))
a1 <- "Amaurornis_cinerea"
crs <- "+proj=longlat +datum=WGS84 +no_defs"
##create world grid 64800
df <- data.frame(x=c(-180,-180,180,180,-180), y=c(-90,90,90,-90,-90)) #5points draw a square
df <- st_as_sf(df, coords=c("x","y"), crs=crs)
bbox.world <- st_as_sfc(st_bbox(df), crs=crs)
grid <- st_make_grid(bbox.world, cellsize = c(1,1), what = "polygons", crs=crs)
grid.sf <- st_as_sf(grid, crs=crs)
grid.sf$ID <- c(1:nrow(grid.sf))##grid needs to be sf
IDsum <- c(NULL) #store the index of grid
sf_use_s2(FALSE) 
for(a1 in a)
{
  if(!file.exists(sprintf("/Volumes/WD6T/Shape/%s/%s.shp", a1, a1))) {next}
  if(!file.exists(sprintf("/Volumes/WD6T/Shape_temp/%s.rda", a1)))
  {
    saveRDS(NULL, sprintf("/Volumes/WD6T/Shape_temp/%s.rda", a1))
    shp <- st_read(sprintf("/Volumes/WD6T/Shape/%s/%s.shp", a1, a1))
    shp <- shp[which(st_geometry_type(shp)=='MULTIPOLYGON'|st_geometry_type(shp)=='POLYGON'),]
    if(nrow(shp)==0){next}
    bbox <- st_bbox(shp)
    bbox[c(1,2)] <- floor(bbox[c(1,2)])-0.01 #### 0≠0 
    bbox[c(3,4)] <- ceiling(bbox[c(3,4)])+0.01 ####
    if(abs(bbox[1])>180) #boarder
    {
      bbox[1] <- -179.9
    }
    if(abs(bbox[3])>180)
    {
      bbox[3] <- 179.9
    }
    bbox.polygon <- st_as_sfc(bbox, crs = st_crs(shp))
    st.contain <- st_intersects(bbox.polygon, grid.sf)[[1]]
    subgrid <- grid.sf[st.contain,] 
    grid.inter <- st_intersects(shp, subgrid)
    grid.inter <- unlist(grid.inter) %>% unique()
    subgrid <- subgrid[grid.inter,]
    IDs <- subgrid$ID 
    saveRDS(IDs, sprintf("/Volumes/WD6T/Shape_temp/%s.rda", a1))
  }
}
setwd("/Volumes/WD6T/Shape_temp")
a <- list.files()
a1 <- a[1]
IDsum <- c(NULL)
for(a1 in a)
{
  temp <- readRDS(a1)
  IDsum <- c(IDsum, temp)
  print(which(a1==a))
}
richness <- as.data.frame(table(IDsum))
grid.sf <- merge(grid.sf, richness, by.x="ID", by.y="IDsum", all.x=TRUE)
saveRDS(grid.sf, "/Volumes/WD6T/grid_sf.rda")

###IUCN richness
grid.sf <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/grid_sf.rda")
grid.sf$LONGITUDE <- 0
grid.sf$LATITUDE <- 0
for(i in 1:64800)
{
  grid.sf$LONGITUDE[i] <- grid.sf[[3]][[i]][[1]][1]
  grid.sf$LATITUDE[i] <- grid.sf[[3]][[i]][[1]][6]
  print(i)
}

#eBird richness
sp.num.grid.total <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/sp_totalnumber_grid.rda")
split.col <- strsplit(sp.num.grid.total$coord, " ")
sp.num.grid.total$LONGITUDE <- sapply(split.col, function(x)x[1]) 
sp.num.grid.total$LATITUDE <- sapply(split.col, function(x)x[2])
world.grid <- expand.grid(LONGITUDE=-180:179, LATITUDE=-90:89)
sp.num.grid.total <- sp.num.grid.total[,-1]
sp.num.grid.total <- merge(world.grid, sp.num.grid.total, by=c( "LONGITUDE", "LATITUDE"), all.x=TRUE)
colnames(sp.num.grid.total)[3] <- "N_species_eBird"
grid.sf.1 <- merge(grid.sf, sp.num.grid.total, by=c("LONGITUDE","LATITUDE"))

ob.locality.number <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_locality_number.rda")
ob.locality.number$coord <- paste(ob.locality.number$LONGITUDE, ob.locality.number$LATITUDE)
ob.local.se <- ob.locality.number[,.(N=.N), by=list(coord, LONGITUDE, LATITUDE)]
world.grid <- expand.grid(LONGITUDE=-180:179, LATITUDE=-90:89)
ob.local.se <- merge(world.grid, ob.local.se[,c(2:4)], by=c("LONGITUDE","LATITUDE"), all.x=TRUE)
colnames(ob.local.se)[3] <- "N_observers"
grid.sf.1 <- merge(grid.sf.1, ob.local.se, by=c("LONGITUDE","LATITUDE"))

grid.sf.1[,c(1:7)] -> data
map <- map_data("world")

p1 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= N_species_IUCN))+
  scale_fill_gradient(low = "white", high="darkred", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_minimal()+
  labs(fill="Species richness")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(a) IUCN richness")

p2 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= N_species_eBird))+
  scale_fill_gradient(low = "white", high="darkred", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_minimal()+
  labs(fill="Species richness")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(b) eBird richness")

p.combine <- ggpubr::ggarrange(p1, p2, common.legend = TRUE, legend = "right")

p3 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= log(N_observers)))+
  scale_fill_gradient(low = "white", high="darkblue", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_minimal()+
  labs(fill="Abundance")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(c) Observer abundance")


data$N_species_eBird[is.na(data$N_species_eBird)] <- 0
data$difference <- data$N_species_eBird-data$N_species_IUCN
p5 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= difference))+
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", midpoint = 0, na.value = "transparent", space=.1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_minimal()+
  labs(fill="Number of species")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(d) Richness Difference between IUCN & eBird")

p2.combine <- ggpubr::ggarrange(p3, p5, common.legend = FALSE, legend="right")
pall.combine <- ggpubr::ggarrange(p.combine, p2.combine, ncol=1)
ggsave("Fig6.png")






