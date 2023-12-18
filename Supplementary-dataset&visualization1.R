### Figures and data used
library(data.table)
library(dplyr)
library(ggplot2)

if(F)
{
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
}

###Visualization
###Fig.1
ob <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob.rda")
ob.year <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_year.rda")
ob.month <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_month.rda")

library(tidyr)
ob.long <- pivot_longer(ob[,c(3,4)], cols = c(N_species, N_locality), names_to = "Value_Type", values_to = "Value")
summary(ob$N_species)

#Fig1a
p0 <- ggplot(ob.long, aes(x=Value_Type, y=Value))+
  stat_boxplot(geom = "errorbar", width=.4, aes(x=Value_Type, y=Value))+
  geom_boxplot(aes(fill=Value_Type), outlier.size = .8, outlier.alpha = .5)+
  scale_y_log10()+
  theme_bw()+
  scale_fill_manual(values = c("N_species"="indianred", "N_locality"="steelblue"))+
  xlab(NULL)+
  ylab(NULL)+
  scale_x_discrete(labels=c("Locality","Species"))+
  theme(legend.position = "none")+
  labs(title = "(a) Number of localities visited and species observed of each observer")+
  theme(axis.text.x = element_text(size=12))
p0
#Fig1b,c
p1 <- ggplot(ob.year %>% filter(N_species>=1), aes(x=log(N_species)))+
  geom_histogram(aes(y=stat(density)), fill="indianred")+
  geom_density(aes(y=stat(density)), linewidth=.5)+
  facet_wrap(~Year, ncol = 5)+
  ylab("Frequency")+
  labs(title="(b) Number of species observered (log-transformed)")+
  xlab(NULL)+
  theme_bw()

p2 <- ggplot(ob.year %>% filter(N_locality>=10), aes(x=log(N_locality)))+
  geom_histogram( aes(y=stat(density)), fill="steelblue")+
  geom_density(aes(y=stat(density)), linewidth=.5)+
  facet_wrap(~Year, ncol=5)+
  ylab("Frequency")+
  labs(title="(c) Number of localities (log-transformed)")+
  xlab(NULL)+
  theme_bw()

p.observer <- ggpubr::ggarrange(p1, p2, ncol=1)
p.fig1 <- ggpubr::ggarrange(p0, p.observer, nrow=1)
p.fig1
ggsave("Fig1.png", dpi=300, width = 14, height = 8)
ggsave("Fig1.pdf", dpi=300, width = 14, height = 8)

#Fig2
obser.status <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/obser_nation_status.rda")
status.se <- obser.status[,.(N=.N), by=list(Year, Month, Nation.status)]
status.se$Month <- month.name[status.se$Month]
status.se$date <- paste0(status.se$Year,"-",status.se$Month,"-01")
Sys.setlocale("LC_TIME","C")
status.se$date <- as.Date(status.se$date, format = "%Y-%B-%d")
status.re <- status.se %>% tidyr::spread(key = Nation.status, value = N)

p1 <- ggplot(status.re, aes(x=date))+
  geom_line(aes(y=Local, color="Local"))+
  geom_line(aes(y=International*7, color="International"))+
  scale_y_continuous(name = "Number of Local Observer", sec.axis = sec_axis(~./7, name = "Number of International Observer"))+
  xlab("Date")+
  theme_bw()+
  theme(axis.title.y.right =element_text(angle=90))+
  scale_color_manual(values = c("Local"="steelblue", "International"="indianred"))+
  labs(color="Observer type")+
  theme(
    legend.position = c(.05,.70),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent")
  )


#FigS2
data.se <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/species_status_sum_month.rda")
data.se.se <- data.se[,.(N_sp=.N), by=list(Year, Month, Nation.status)]
data.se.se$date <- as.Date(paste0(data.se.se$Year,"-",data.se.se$Month,"-01"))
data.se.se0 <- unique(data.se[,c(1,2,3)])
data.se.se00 <- data.se.se0[,.(N_spsum=.N), by=list(Year, Month)]
data.se.se <- merge(data.se.se, data.se.se00, by=c("Year","Month"))
data.se$date <- as.Date(paste0(data.se$Year, "-", data.se$Month, "-01"))
##set VU and higher as threatened
data.se <- data.se %>% mutate(Threat = ifelse(Global.IUCN.Red.List.Category %in% c("LC", "NT"), "no","Threatened"))
data.se.fig4 <- data.se[,.(Sum.sp=sum(Sum)), by=list(Year, Month, Nation.status, Threat)]
data.se.fig4$date <- as.Date(paste0(data.se.fig4$Year,"-", data.se.fig4$Month,"-01"))
data.se.fig4.2 <- tidyr::spread(data.se.fig4, key=Nation.status, value = Sum.sp)
data.se.fig4.2$ratio2 <- data.se.fig4.2$International/(data.se.fig4.2$Local+data.se.fig4.2$International)
p <- ggplot(data.se.fig4.2, aes(x=date))+
  geom_line(aes(y=ratio2, group=Threat, color=Threat))+
  theme_bw()+
  xlab("Date")+
  ylab("Percentage of International Observation")+
  labs(color="Species status")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("no"="steelblue", "Threatened"="indianred"), labels = c("no"="Not Threatened", "Threatened"="Threatened"))+
  theme(
    legend.position = c(.05,.7),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent")
  )
p
#FigS1
data.se <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/species_status_sum_month.rda")
data.se.se <- data.se[,.(N_sp=.N), by=list(Year, Month, Nation.status)]
data.se.se$date <- as.Date(paste0(data.se.se$Year,"-",data.se.se$Month,"-01"))
data.se.se0 <- unique(data.se[,c(1,2,3)])
data.se.se00 <- data.se.se0[,.(N_spsum=.N), by=list(Year, Month)]
data.se.se <- merge(data.se.se, data.se.se00, by=c("Year","Month"))

p2 <- ggplot(data.se.se, aes(x=date, fill=Nation.status))+
  geom_bar(aes(y=N_sp), position = "dodge", stat="identity")+
  theme_bw()+
  ylab("Number of species recorded")+
  geom_line(aes(y=N_spsum, color="Total Species Number"),linetype="dashed", size=.5)+
  coord_cartesian(ylim = c(3000,9500))+
  xlab("Date")+
  scale_fill_manual(values = c("Local"="steelblue", "International"="indianred"))+
  labs(fill="Observer type")+
  theme(
    legend.position = c(.05,.85),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent")
  )+
  guides(
    color = guide_legend(order = 2, nrow = 1, title = ""),
    fill = guide_legend(order = 1, nrow = 1)
  )+
  theme(legend.box = "horizontal")+
  scale_color_manual(values = "gray")


#Fig3
##Data preparation part1
if(F)
{
  
  ###IUCN & eBird richness
  #Find shared species list
  setwd("/Volumes/WD6T/Shape")
  a <- list.files()
  splist.iucn <- gsub("_"," ",a)
  species.locality.year <- readRDS("/Volumes/WD6T/species_locality_year.rda")
  sp.locality.year <- unique(species.locality.year[,c(1,2,4)])
  splist.ebird <- unique(sp.locality.year$SCIENTIFIC_NAME)
  overlap.specielist <- intersect(splist.iucn, splist.ebird)
  unique.ebird <- setdiff(splist.ebird, splist.iucn)
  unique.iucn <- setdiff(splist.iucn, splist.ebird)
  namelist <- read.csv("/Volumes/WD6T/namelist.csv")
  namelist <- unique(namelist[,c(1,2)]) %>% na.omit()
  unique.iucn <- data.frame(Species1_BirdLife=unique.iucn)
  unique.iucn <- merge(unique.iucn, namelist, by="Species1_BirdLife", all.x=TRUE)
  iucn.match.ebird <- intersect(unique.iucn$Species2_eBird, unique.ebird)
  ebird.match.iucn <- unique.iucn %>% filter(Species2_eBird %in% iucn.match.ebird) ##636 species in IUCN match 601 species in eBird
  ebird.match.iucn <- ebird.match.iucn[!duplicated(ebird.match.iucn$Species2_eBird) & !duplicated(ebird.match.iucn$Species2_eBird, fromLast = TRUE),]
  overlap.specielist.ebird <- c(overlap.specielist, ebird.match.iucn$Species2_eBird)
  overlap.specielist.iucn <- c(overlap.specielist, ebird.match.iucn$Species1_BirdLife)
  ##10130 species share
  saveRDS(overlap.specielist.ebird, "/Volumes/WD6T/shared_species_list_ebird.rda")
  saveRDS(overlap.specielist.iucn, "/Volumes/WD6T/shared_species_list_iucn.rda")
  ###
  #eBird richness per grid
  species.locality.year <- readRDS("/Volumes/WD6T/species_locality_year.rda")
  overlap.specieslist.ebird <- readRDS("/Volumes/WD6T/shared_species_list_ebird.rda")
  sp.locality.year <- unique(species.locality.year[,c(1,2,4)])
  sp.locality.year <- sp.locality.year %>% filter(SCIENTIFIC_NAME %in% overlap.specielist.ebird)
  sp.locality.year$coord <- paste(sp.locality.year$LONGITUDE, sp.locality.year$LATITUDE)
  sp.locality.year <- sp.locality.year[,c(4,3)]
  sp.totalnumber.grid <- sp.locality.year[,.(N_species=.N), by=list(coord)]
  saveRDS(sp.totalnumber.grid, "/Volumes/WD6T/sp_totalnumber_grid.rda")
  #IUCN richness per grid
  if(F)
  {
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
  }
}

##Data preparation part2
if(F)
{
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
  sp.num.grid.total$LONGITUDE <- sapply(split.col, function(x)x[1]) #没看懂的语法
  sp.num.grid.total$LATITUDE <- sapply(split.col, function(x)x[2])
  world.grid <- expand.grid(LONGITUDE=-180:179, LATITUDE=-90:89)##改一改
  sp.num.grid.total <- sp.num.grid.total[,-1]
  sp.num.grid.total <- merge(world.grid, sp.num.grid.total, by=c( "LONGITUDE", "LATITUDE"), all.x=TRUE)
  colnames(sp.num.grid.total)[3] <- "N_species_eBird"
  grid.sf.1 <- merge(grid.sf, sp.num.grid.total, by=c("LONGITUDE","LATITUDE"))
  
  ##每个格子里的人数
  ob.locality.number <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/ob_locality_number.rda")
  ob.locality.number$coord <- paste(ob.locality.number$LONGITUDE, ob.locality.number$LATITUDE)
  ob.local.se <- ob.locality.number[,.(N=.N), by=list(coord, LONGITUDE, LATITUDE)]
  world.grid <- expand.grid(LONGITUDE=-180:179, LATITUDE=-90:89)
  ob.local.se <- merge(world.grid, ob.local.se[,c(2:4)], by=c("LONGITUDE","LATITUDE"), all.x=TRUE)
  colnames(ob.local.se)[3] <- "N_observers"
  grid.sf.1 <- merge(grid.sf.1, ob.local.se, by=c("LONGITUDE","LATITUDE"))
  
  ##每个格子的记录数
  obv.locality <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/observation_locality.rda")
  obv.locality$coord <- paste(obv.locality$LONGITUDE, obv.locality$LATITUDE)
  obv.locality.year <- obv.locality
  obv.locality <- obv.locality[,.(N=sum(N_observation)), by=list(coord, LONGITUDE, LATITUDE)]
  world.grid.year <- expand.grid(Year=2018:2022, LONGITUDE=-180:179, LATITUDE=-90:89)
  obv.locality.year <- merge(world.grid.year, obv.locality.year, by=c("Year", "LONGITUDE", "LATITUDE"), all.x=TRUE)
  world.grid <- expand.grid(LONGITUDE=-180:179, LATITUDE=-90:89)
  obv.locality <- merge(world.grid, obv.locality, by=c("LONGITUDE", "LATITUDE"), all.x = TRUE)
  obv.locality <- obv.locality[,-3]
  colnames(obv.locality)[3] <- "N_observations"
  grid.sf.1 <- merge(grid.sf.1, obv.locality, by=c("LONGITUDE","LATITUDE"))
  colnames(grid.sf.1)[4] <- "N_species_IUCN"
  ####开始画图
  ggplot(grid.sf.1)+
    geom_sf(aes(fill=N_species_IUCN), col="transparent")+
    scale_fill_gradient(low = "grey", high = "darkblue", na.value = "transparent")
  
  grid.sf.1[,c(1:7)] -> data
  saveRDS(data, "/Users/tangerine/Documents/Lab_data/data_map.rda")
}
#Visualization
data <- readRDS("/Users/tangerine/Documents/Lab_data/data_map.rda")
map <- map_data("world")
#F3a
p1 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= N_species_IUCN))+
  scale_fill_gradient(low = "white", high="darkred", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_bw()+
  labs(fill="Species richness")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(a) IUCN richness")+
  theme(
    legend.position = c(.05,.15),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
p1
#F3b
p2 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= N_species_eBird))+
  scale_fill_gradient(low = "white", high="darkred", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_bw()+
  labs(fill="Species richness")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(b) eBird richness")+
  theme(
    legend.position = c(.05,.15),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
p2
#p.combine <- ggpubr::ggarrange(p1, p2, common.legend = TRUE, legend = "right")
#F3C
p3 <- ggplot()+
  geom_tile(data=data, aes(x=LONGITUDE, y=LATITUDE, fill= log(N_observers)))+
  scale_fill_gradient(low = "white", high="darkblue", na.value = "white", space = .1)+
  geom_polygon(data=map %>% filter(long<=179), aes(x=long, y=lat, group=group), color="Black", linewidth=.1, fill="transparent")+
  coord_equal()+
  theme_bw()+
  labs(fill="Abundance")+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "(c) Observer abundance")+
  theme(
    legend.position = c(.05,.15),
    legend.justification = c(0,0),
    legend.background = element_rect(fill="transparent"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
p3
p.fig3 <- cowplot::plot_grid(p1,p2,p3,ncol=1, align = "v")
p.fig3

##FigS3
data <- readRDS("/Users/tangerine/Documents/Lab_data/ebird_covid/species_country_new_status_sum.rda")
#table(data$COUNTRY)
#length(unique(data$COUNTRY))
data.1 <- data
data.1.se <- data.1[,.(Nsum=sum(N.sum)), by=list(COUNTRY, Nation.status,Year)]
library(countrycode)
data.1.se$continent <- countrycode(data.1.se$COUNTRY,"country.name","continent")
data.1.se[which(data.1.se$continent=="Asia")]$continent <- countrycode(data.1.se[which(data.1.se$continent=="Asia")]$COUNTRY,"country.name","un.regionsub.name")
#table(data.1.se$continent)
data.1.se[which(data.1.se$continent=="Europe")]$continent <- countrycode(data.1.se[which(data.1.se$continent=="Europe")]$COUNTRY,"country.name","un.regionsub.name")


p1 <- ggplot(data.1.se %>% filter(continent=="South-eastern Asia"))+
  geom_bar(stat = "identity", position = "fill", aes(y=COUNTRY, x=Nsum, fill=Nation.status))+
  facet_wrap(~Year, ncol = 5)+
  ggtitle("(a) Southeast Asia")+
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

p.combine <- ggpubr::ggarrange(p1, p2, nrow = 2, ncol = 1, common.legend = TRUE, legend="bottom")
p.combine




