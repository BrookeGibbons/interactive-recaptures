# Create html leaflet maps to show fishers
rm(list=ls()) # Clears memory

# librarys----
library(stringr)
library(tidyverse)
library(leaflet)

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Read in data ----
setwd(data.dir)
dir()

length<-read.csv("length.csv")%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Sex,Colour,Outlier,Carapace.length)%>%
  dplyr::filter(!Tag.number%in%c("K3211"))%>%
  dplyr::filter(!is.na(Tag.number))%>%
  glimpse()

metadata<-read.csv("metadata.csv")%>%
  dplyr::filter(!Successful.count%in%c("No"))%>%
  dplyr::distinct(Source,Sample,Location,Latitude,Longitude,Date)%>%
  dplyr::filter(!Source%in%c("ben-seven-mile"))%>%
  glimpse()

length(unique(metadata$Sample)) # 2116

# Join data together ----
dat<-semi_join(length,metadata)%>%
  left_join(.,metadata)%>%
  dplyr::filter(!is.na(Latitude))%>%
  mutate(Latitude=jitter(Latitude))%>% # Jitter points so they arent the same
  mutate(Longitude=jitter(Longitude)) # maybe do this after working out distance between points

summary(dat)

length(unique(dat$Sample)) # 1526

# Create a releases and a recaptures dataframe ----
dat.recaptures<-dat%>%
  dplyr::filter(Recapture%in%c("TRUE"))%>%
  dplyr::select(-c(Source,Sample,Recapture,Outlier,Location,Sex))%>%
  glimpse()

dat.releases<-dat%>%
  dplyr::filter(!Recapture%in%c("TRUE"))%>%
  glimpse()

# Fix issues in recaptures (caught twice in one day) ----
# First fix tags that are caught on the same date twice
double.release.same.date<-dat.releases%>%
  dplyr::group_by(Tag.number,Date)%>%
  dplyr::summarise(n=n())%>%
  filter(n>1) # 25 are incorrect tags

# Remove from the data 
dat.releases<-dat.releases%>%
  filter(!Tag.number%in%c(as.vector(double.release.same.date$Tag.number)))

# See what has been released twice
double.release<-dat.releases%>%
  dplyr::group_by(Tag.number)%>%
  dplyr::summarise(n=n())%>%
  filter(n>1) # 60 are incorrect tags

extra.recaps<-dat.releases%>%
  filter(Tag.number%in%c(as.vector(double.release$Tag.number)))

# Decided I am going to just filter these out - maybe discuss with Tim if they should be added back in ----
dat.releases<-dat.releases%>%
  filter(!Tag.number%in%c(as.vector(double.release$Tag.number)))

# Create dataframe for releases that are recaptured to join back into recaptures ----
dat.releases.caught.again<-semi_join(dat.releases,dat.recaptures,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Outlier,Recapture))%>%
  glimpse()

# create initial release data for each lobster ----
release.locations.and.sex<-semi_join(dat.releases,dat.recaptures,by="Tag.number")%>%
  distinct(Tag.number,Location,Sex,Colour)%>%
  dplyr::rename(Release.location=Location,Release.colour=Colour)%>%
  glimpse()

# Create size classes ----
size.at.release<-semi_join(dat.releases,dat.recaptures,by="Tag.number")%>%
  dplyr::select(Tag.number,Carapace.length)%>%
  dplyr::mutate(Size.class.at.release=ifelse(Carapace.length<76,"Sub-legal","Legal"))%>%
  dplyr::rename(Carapace.length.at.release=Carapace.length)%>%
  dplyr::select(Tag.number,Size.class.at.release,Carapace.length.at.release)

size.at.last.recapture<-dat.recaptures%>%
  dplyr::arrange(Tag.number,desc(Date))%>%
  dplyr::group_by(Tag.number)%>%
  slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Size.class.at.last.recapture=ifelse(Carapace.length<76,"Sub-legal","Legal"))%>%
  dplyr::rename(Carapace.length.at.last.recapture=Carapace.length)%>%
  dplyr::select(Tag.number,Size.class.at.last.recapture,Carapace.length.at.last.recapture)

size.classes<-left_join(size.at.release,size.at.last.recapture)

# Create recapture data for the map ----
# Good tag to check 191410 (to see if everything has worked)
dat.map.recaptures<-dat.releases.caught.again%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date,Release.colour=Colour,Release.location=Location,Release.carapace.length=Carapace.length)%>% #
  left_join(dat.recaptures)%>%
  bind_rows(dat.releases.caught.again)%>%
  arrange(Tag.number,Date)%>%
  dplyr::group_by(Tag.number)%>%
  dplyr::mutate(Release.latitude=lag(Latitude))%>% # Need to lobsters caught more than twice
  dplyr::mutate(Release.longitude=lag(Longitude))%>%
  dplyr::mutate(Release.date=lag(Date))%>% # To get last time it was caught
  dplyr::mutate(Release.carapace.length=lag(Carapace.length))%>% # To get last time it was caught
  dplyr::filter(!is.na(Sex))%>% # Get rid of initial ones
  dplyr::filter(!is.na(Release.colour))%>% # Get rid of initial ones
  dplyr::rename(Recapture.latitude=Latitude,Recapture.longitude=Longitude,Recapture.date=Date,Recapture.colour=Colour,Recapture.carapace.length=Carapace.length)%>%
  dplyr::mutate(Days.between.captures = (as.Date(as.character(Recapture.date), format="%Y-%m-%d")) - (as.Date(as.character(Release.date))))%>%
  dplyr::mutate(Growth=Recapture.carapace.length-Release.carapace.length)%>%
  dplyr::ungroup()%>%
  glimpse()

# Create initial release data for the map ----
dat.map.releases<-semi_join(dat.releases,dat.recaptures,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Outlier,Location,Recapture))%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date,Release.colour=Colour,Release.carapace.length=Carapace.length)%>%
  glimpse()

# create labels for recaptures ----
recapture.labels<-dat.map.recaptures%>%
  dplyr::mutate(Labels=paste("<b>Tag: </b>",Tag.number,"<br>",
                             "<b>Sex: </b>",Sex,"<br>",
                             "<b>Colour: </b>",Recapture.colour,"<br>",
                             "<b>Current carapace length: </b>",Recapture.carapace.length," mm","<br>",
                             "<b>Release date: </b>",Release.date,"<br>",
                             "<b>Recapture date: </b>",Recapture.date,"<br>",
                             "<b>Growth: </b>",Growth," mm","<br>",
                             "(",Days.between.captures," days between captures)",sep=""))

# create labels for releases ----
releases.labels<-dat.map.releases%>%
  dplyr::mutate(Labels=paste("<b>Tag: </b>",Tag.number,"<br>",
                             "<b>Sex: </b>",Sex,"<br>",
                             "<b>Colour: </b>",Release.colour,"<br>",
                             "<b>Carapace length: </b>",Release.carapace.length," mm","<br>",
                             "<b>Release date: </b>",Release.date,"<br>"))

# Create point data ----
glimpse(dat.map.releases)
glimpse(dat.map.recaptures)

points.recaptures<-dat.map.recaptures%>%
  left_join(.,recapture.labels)%>%
  dplyr::rename(Latitude=Recapture.latitude,Longitude=Recapture.longitude)%>%
  left_join(.,size.classes)

points.releases<-dat.map.releases%>%
  left_join(.,releases.labels)%>%
  dplyr::rename(Latitude=Release.latitude,Longitude=Release.longitude)%>%
  #dplyr::select(Tag.number,Latitude,Longitude,Labels,Carapace.length,Release.colour)%>%
  left_join(.,size.classes)

dat.map.all<-bind_rows(points.recaptures,points.releases)%>%
  left_join(.,release.locations.and.sex)%>%
  arrange((Latitude))%>%
  mutate(Release.location = factor(Release.location, levels = c("Seven Mile","Rivermouth","Irwin Reef", "Cliff Head","Golden Ridge")))%>%
  glimpse()

unique(dat.map.all$Release.location)

sex.pal <- colorFactor(c( "hotpink","blue"), domain = c("Male", "Female"))

# http://tools.medialab.sciences-po.fr/iwanthue/
loc.pal <- colorFactor(c("#d37083","#d09148","#e1d17d","#92c46a","#af76d5"),domain=dat.map.all$Release.location)

map.all.leaflet <- leaflet(dat.map.all) %>% 
  # Set Box
  fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
  
  # Add background maps
  addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean basemap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Black and white")%>%
  addTiles(group="Open street map")%>% 
  
  # All data
  addCircleMarkers(data=dat.map.all,~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 1,popup = ~as.character(Labels),group="All sizes")%>%
  
  
  # Sub legal
  addCircleMarkers(data=dat.map.sub.legal,~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 1,popup = ~as.character(Labels),group="Sub-legal")%>%
  
  # Legal
  addCircleMarkers(data=dat.map.legal,~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 0.5,popup = ~as.character(Labels),group="Legal")%>%
  
  # Changed size
  addCircleMarkers(data=dat.map.sub.legal.to.legal,~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),fillOpacity = 0.5,popup = ~as.character(Labels),stroke = TRUE,weight=1,group="Sub-legal to legal")%>% 
  
  # Legend
  addLegend("bottomright", pal = loc.pal, values = ~dat.map.all$Release.location, title = "Release Location",opacity = 1)%>%# 
  # Inset Map
  addMiniMap(position = "bottomleft")%>% 
  # Measure tool
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")%>%
  # Control Layers
  addLayersControl(
    baseGroups = c("Ocean basemap","World imagery","Open street map","Black and white"),
    overlayGroups = c("All sizes","Legal","Sub-legal","Sub-legal to legal"),
    options = layersControlOptions(collapsed = FALSE))%>%
  # Scale Bar
  addScaleBar()


map.all.leaflet  

# Combine recaptures and releases together to make a dataframe that has all lines ----
dat.map.all<-bind_rows(dat.map.releases,dat.map.recaptures)%>%
  arrange(Tag.number,Release.date)%>%
  dplyr::mutate(Release.latitude=ifelse(((!Release.latitude==Recapture.latitude)&(!is.na(Recapture.latitude))),Recapture.latitude,Release.latitude))%>% # So if release and recapture are both not na and are different then release = recapture this is two get a line when only recaptured once
  dplyr::mutate(Release.longitude=ifelse(((!Release.longitude==Recapture.longitude)&(!is.na(Recapture.longitude))),Recapture.longitude,Release.longitude))%>%
  mutate(num=1:nrow(.))%>%
  mutate(Progress=round((num/nrow(.))*100,digits=2))%>%
  left_join(.,sizes)%>%
  glimpse()


# Loop through tags to add to map ----
uniq <- unique(unlist(dat.map.all$Tag.number))
uniq

for (i in 1:length(uniq)){
  
  temp.dat <- subset(dat.map.all, Tag.number == uniq[i])
  print(paste("Plotting ",unique(temp.dat$Tag.number),", ",unique(temp.dat$Progress),"% finished",sep=""))
  
  map.all.leaflet <- map.all.leaflet%>%
    addPolylines(data=temp.dat, ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = "black",group="All sizes")%>%
    
    addPolylines(data=filter(temp.dat,Size%in%c("Sub-legal")), ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = "black",group="Sub-legal")%>%
    addPolylines(data=filter(temp.dat,Size%in%c("Legal")), ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = "black",group="Legal")%>%
    addPolylines(data=filter(temp.dat,Size%in%c("Changed to legal size")), ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = "black",group="Sub-legal to legal")
}

map.all.leaflet



