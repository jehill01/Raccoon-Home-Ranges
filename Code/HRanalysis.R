library(emmeans)
library(lme4)
library(ggplot2)
library(dplyr)
library(MuMIn)
library(stringr)
library(MuMIn)
library(sp)
library(lubridate)
library(rgdal)
library(move)
library(sjPlot)
library(ggeffects)

#STEP 1- Calculate UD Sizes for the newer animals
data<-read.csv("alldata.csv")
data$Seconds<-paste("0")
data<-data %>% mutate(Timestamp = case_when(Year == 21 ~ 
                                              paste(as.Date(data$Day, origin="2020/12/31"),"-",data$Hour,'-',data$Minutes,'-',data$Seconds),
                                            Year == 22 ~ paste(as.Date(data$Day, origin="2021/12/31"),"-",data$Hour,'-',data$Minutes,'-',data$Seconds)))

data$Timestamp<-ymd_hms(data$Timestamp)
data$Month<-format(data$Timestamp, "%m")
head(data)
data<-data[!duplicated(data[c("ID","Timestamp")]),]
coordinates(data) <- c("coords.x1", "coords.x2")
proj4string(data) <- CRS( "+proj=longlat +datum=WGS84" )
data2<-as.data.frame(spTransform(data, CRS('EPSG: 26917')))
data3<-data2[with(data2, order(collar, Timestamp)),]
raccoons<-move(x=data3$coords.x1, y=data3$coords.x2, animal=data3$ID, data=data3, time=data3$Timestamp)
datalist<-split(raccoons, list(data3$ID, data3$Month, data3$Year), drop = TRUE)
as.data.frame(names(which(lapply(datalist, length)<30)))

datalist$`907.02.21`<-NULL
datalist$`913.02.21`<-NULL
datalist$`914.02.21`<-NULL
datalist$`955.10.21`<-NULL
datalist$`822.01.22`<-NULL
datalist$`913.01.22`<-NULL
datalist$`822.02.22`<-NULL
datalist$`975.03.22`<-NULL
datalist$`908.04.22`<-NULL
datalist$`909.04.22`<-NULL
datalist$`936.04.22`<-NULL
datalist$`982.04.22`<-NULL

modlist95<-list()
udlist95<-list()
plotlist95<-list()
arealist95<-list()

for (i in names(datalist) ){ ## i=names(datalist)[2]
  modlist95[[i]]<-brownian.bridge.dyn(datalist[[i]], 
                                      location.error=7, raster=50, ext=5, margin=5, window=11)
  flush.console()
  udlist95[[i]]<-getVolumeUD(modlist95[[i]])
  plotlist95[[i]]<-udlist95[[i]]<=0.95
  arealist95[[i]]<-sum(values(plotlist95[[i]]))
}


#Ouput of arealist is number of raster cells, so
# multiply area by the size of the raster cells then 
# divide by 1e06 to get sq km#
Area<-lapply(arealist95, function(x) (x*2500)/1000000) 
Area2<-as.data.frame(unlist(Area)) #get the areas into a df#
Individual<-as.data.frame(names(datalist))
data3$name<-gsub(" ", "", paste(data3$ID,".",data3$Month,".",data3$Year))
AreaDF<-merge.data.frame(data.frame(cbind(Area2, Individual)), 
                         data3, by.x="names.datalist.", by.y="name" )

modlist60<-list()
udlist60<-list()
plotlist60<-list()
arealist60<-list()

for (i in names(datalist) ){ ## i=1
  modlist60[[i]]<-brownian.bridge.dyn(datalist[[i]], 
                                      location.error=7, raster=50, ext=5, margin=5, window=11,verbose=F)
  flush.console()
  udlist60[[i]]<-getVolumeUD(modlist60[[i]])
  plotlist60[[i]]<-udlist60[[i]]<=0.60
  arealist60[[i]]<-sum(values(plotlist60[[i]]))
}




Area60<-lapply(arealist60, function(x) (x*2500)/1000000) 
Area260<-as.data.frame(unlist(Area60))
AreaDF2<-merge.data.frame(data.frame(cbind(Area260, Individual)), 
                          AreaDF, by.x="names.datalist.", by.y="names.datalist." )

names(AreaDF2)[names(AreaDF2) == "names.datalist."] <- "Individual"
names(AreaDF2)[names(AreaDF2) == "coords.x1"] <- "Easting"
names(AreaDF2)[names(AreaDF2) == "coords.x2"] <- "Northing"
names(AreaDF2)[names(AreaDF2) == "unlist.Area."] <- "UD95"
names(AreaDF2)[names(AreaDF2) == "unlist.Area60."] <- "UD60"
AreaDF2$sex[AreaDF2$sex == "male"] <- "Male"
AreaDF2$sex[AreaDF2$sex == "female"] <- "Female"
AreaDF2$habitat[AreaDF2$habitat == "bottomland"] <- "Bottomland"
AreaDF2$habitat[AreaDF2$habitat == "pine/wetland"] <- "Pine/wetland"
AreaDF2$habitat[AreaDF2$habitat == "riparian"] <- "Riparian"


kernels<-subset(AreaDF2, !duplicated(Individual)) #Keeping one per animal per month
UDNew<-kernels[,c("Individual", "UD60", "UD95", "collar", "habitat", "sex", "Timestamp", "Month", "Year", "ID")]

#STEP 2- Repeat of above with translocated animals

data<-read.csv("precontrolcollar.csv")
head(data)
tags<-read.csv("tags.csv")
data$Timestamp<-as.POSIXct(data$Timestamp, tz="EST", 
                           format="%m/%d/%Y %H:%M:%S")

data$Month<-format(data$Timestamp, "%m")
data$Year<-format(data$Timestamp, "%y")
data2<-data %>% left_join(tags, by="collarID")
data<-data2
names(data)[names(data) == "ID.y"] <- "ID"
data<-data[!duplicated(data[c("ID","Timestamp")]),]
coordinates(data) <- c("Easting", "Northing")
proj4string(data) <- CRS( "+proj=longlat +datum=WGS84" )
data2<-as.data.frame(spTransform(data, CRS('EPSG: 26917')))
data3<-data2[with(data2, order(ID, Timestamp)),]
raccoons<-move(x=data3$coords.x1, y=data3$coords.x2, animal=data3$ID, data=data3, time=data3$Timestamp)
datalist<-split(raccoons, list(data3$ID, data3$Month, data3$Year), drop = TRUE)
as.data.frame(names(which(lapply(datalist, length)<30)))


datalist$`85.09.18`<-NULL #Removing ones with less than 30 points
datalist$`87.09.18`<-NULL
datalist$`93.09.18`<-NULL
datalist$`526.09.18`<-NULL
datalist$`82.11.18`<-NULL
datalist$`85.12.18`<-NULL
datalist$`87.12.18`<-NULL
datalist$`499.12.18`<-NULL
datalist$`99.01.19`<-NULL
datalist$`85.02.19`<-NULL
datalist$`85.04.19`<-NULL
datalist$`100.01.19`<-NULL
names(datalist)
modlist95<-list()
udlist95<-list()
plotlist95<-list()
arealist95<-list()

for (i in names(datalist) ){ ## i=names(datalist)[2]
  modlist95[[i]]<-brownian.bridge.dyn(datalist[[i]], 
                                      location.error=7, raster=50, ext=5, margin=5, window=11)
  flush.console()
  udlist95[[i]]<-getVolumeUD(modlist95[[i]])
  plotlist95[[i]]<-udlist95[[i]]<=0.95
  arealist95[[i]]<-sum(values(plotlist95[[i]]))
}

#Ouput of arealist is number of raster cells, so
# multiply area by the size of the raster cells then 
# divide by 1e06 to get sq km#
Area<-lapply(arealist95, function(x) (x*2500)/1000000) 
Area2<-as.data.frame(unlist(Area)) #get the areas into a df#
Individual<-as.data.frame(names(datalist))
data3$name<-gsub(" ", "", paste(data3$ID,".",data3$Month,".",data3$Year))
AreaDF<-merge.data.frame(data.frame(cbind(Area2, Individual)), 
                         data3, by.x="names.datalist.", by.y="name" )

modlist60<-list()
udlist60<-list()
plotlist60<-list()
arealist60<-list()

for (i in names(datalist) ){ ## i=1
  modlist60[[i]]<-brownian.bridge.dyn(datalist[[i]], 
                                      location.error=7, raster=50, ext=5, margin=5, window=11,verbose=F)
  flush.console()
  udlist60[[i]]<-getVolumeUD(modlist60[[i]])
  plotlist60[[i]]<-udlist60[[i]]<=0.60
  arealist60[[i]]<-sum(values(plotlist60[[i]]))
}




Area60<-lapply(arealist60, function(x) (x*2500)/1000000) 
Area260<-as.data.frame(unlist(Area60))
AreaDF2<-merge.data.frame(data.frame(cbind(Area260, Individual)), 
                          AreaDF, by.x="names.datalist.", by.y="names.datalist." )

names(AreaDF2)[names(AreaDF2) == "names.datalist."] <- "Individual"
names(AreaDF2)[names(AreaDF2) == "SEX"] <- "sex"
names(AreaDF2)[names(AreaDF2) == "unlist.Area."] <- "UD95"
names(AreaDF2)[names(AreaDF2) == "unlist.Area60."] <- "UD60"
names(AreaDF2)[names(AreaDF2) == "Treatment"] <- "habitat"
AreaDF2$sex[AreaDF2$sex == "M"] <- "Male"
AreaDF2$sex[AreaDF2$sex == "F"] <- "Female"

AreaDF2<- AreaDF2 %>% mutate(habitat =
                               case_when(
                                 habitat=="H2L"  ~ "Bottomland",
                                 habitat=="H2H"  ~ "Bottomland", 
                                 habitat=="L2L"  ~ "Pine/wetland",
                                 habitat=="L2H"  ~ "Pine/wetland",
                                 collarID=="40993"~ "Pine/wetland",
                                 collarID=="41001"~ "Pine/wetland",
                                 collarID=="41010"~ "Pine/wetland",
                                 collarID=="41012"~ "Bottomland",
                                 collarID=="41015"~ "Bottomland",
                                 collarID=="50000"~ "Pine/wetland",
                                 collarID=="70000"~ "Pine/wetland"))

kernels2<-subset(AreaDF2, !duplicated(Individual))
names(kernels2)[names(kernels2) == "collarID"] <- "collar"
UDTranslocation<-kernels2[,c("Individual", "UD60", "UD95", "collar", "habitat", "sex", "Timestamp", "Month", "Year", "ID")]
UDTranslocation$Year<-as.integer(UDTranslocation$Year)
UDTranslocation$collar<-as.character(UDTranslocation$collar)
analysisdataset<-bind_rows(UDNew, UDTranslocation)

#STEP 3- analyze differences in HR size
data<-analysisdataset
data$habitat[data$habitat == "Pine/wetland"] <- "Pine"
data<-data %>% mutate(Season =
                              case_when(Month=="01" ~ "Fall",
                                        Month=="02" ~ "Breeding",
                                        Month=="03" ~ "Breeding",
                                        Month=="04" ~ "Breeding",
                                        Month=="05" ~ "Breeding",
                                        Month=="06" ~ "Summer",
                                        Month=="07" ~ "Summer",
                                        Month=="08" ~ "Summer",
                                        Month=="09" ~ "Summer",
                                        Month=="10" ~ "Fall",
                                        Month=="11" ~ "Fall",
                                        Month=="12" ~ "Fall",))

data$Season <- factor(data$Season, levels=c("Breeding", "Summer", "Fall"))
data<-data %>% mutate(Session =
                        case_when(Year=="18" ~ "old",
                                  Year=="19" ~ "old",
                                  Year=="21" ~ "new",
                                  Year=="22"~"new",))
data %>%  group_by(habitat) %>% 
  tally() %>% print(n=98)

data %>% #Table of home range averages
  group_by(habitat, Season, sex) %>%
  summarise(Mean95=mean(UD95),
            sd95=sd(UD95),
            mean60=mean(UD60),
            sd60=sd(UD60),
  )%>% print(n=98)


#95% UD analysis
model95<-lmer(log(UD95)~sex*habitat*Season+(1|ID), data=data, na.action=na.pass, REML=F)
dredge(model95)
r.squaredGLMM(model95)

#Pairwise comparisons
emmeans(model95, pairwise~sex|Season|habitat, type="response")
emmeans(model95, pairwise~habitat|sex|Season, type="response")
emmeans(model95, pairwise~Season|sex|habitat, type="response")
emmip(model95, ~sex|habitat|Season, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")
emmip(model95, ~habitat|sex|Season, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")

#Predicted difference in home range size
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Breeding"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Summer"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Fall"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Breeding"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Summer"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Fall"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Breeding"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Summer"))
ggpredict(model95, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Fall"))

#60% UD sizes
model60<-lmer(log(UD60)~sex*habitat*Season+(1|ID:Session), data=data, na.action=na.pass, REML=F)
dredge(model60)

#Pairwise comparisons
emmeans(model60, pairwise~sex|habitat|Season, type="response")
emmeans(model60, pairwise~habitat|sex|Season, type="response")
emmeans(model60, pairwise~Season|habitat|sex, type="response")
emmip(model60, ~sex|habitat|Season, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")

#Predicted difference in home range size
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Breeding"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Summer"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Bottomland", Season="Fall"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Breeding"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Summer"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Pine", Season="Fall"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Breeding"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Summer"))
ggpredict(model60, ci.lvl=0.95, terms=c("sex"), condition = c(habitat="Riparian", Season="Fall"))

write.csv(emmip( model95 , ~habitat|sex|Season , plotit = FALSE, type="response" ), "gdata95.csv") #These files are used in figures
write.csv(emmip( model60 , ~habitat|sex|Season , plotit = FALSE, type="response" ), "gdata60.csv")

