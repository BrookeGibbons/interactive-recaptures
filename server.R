library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Read ub data ----
points<-read.csv("leaflet.point.data.csv")
lines<-read.csv("leaflet.line.data.csv")


loc.pal <- colorFactor(c("#d37083","#d09148","#e1d17d","#92c46a","#af76d5"),domain=points$Release.location)

function(input, output, session) {

reactiveDf <- reactive({
    if (input$Sex != "") {
      points <- points %>%
        filter(
          Sex %in% input$Sex
        )
      
    }
    
    if (input$Size != "") {
      points <- points %>%
        filter(
          Size %in% input$Size
        )
      
    }
    
    return(points)
  })

    

## Interactive Map ----
# Create the map
output$map <- renderLeaflet({
    
    # Add different base maps
    if (input$base_map == "Ocean basemap") {
      leaflet(points) %>% 
        addCircleMarkers(data=filter(points,Sex%in%input$Sex),~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 0.5,popup = ~as.character(Labels))%>%
        # Set Box
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
        addProviderTiles(providers$Esri.OceanBasemap)%>%
        #addLegend("bottomright", pal = loc.pal, values = ~dat.map.all$Release.location, title = "Release Location",opacity = 1)%>%# 
        # Inset Map
        addMiniMap(position = "topleft")%>% 
        # Measure tool
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479")%>%
        addScaleBar(position = "bottomleft")%>%
        # Legend
        addLegend("bottomleft", pal = loc.pal, values = ~points$Release.location, title = "Release Location",opacity = 1)
      
    } else if (input$base_map == "World imagery") {
      leaflet(points) %>% 
        addCircleMarkers(data=filter(points,Sex%in%input$Sex),~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 0.5,popup = ~as.character(Labels))%>%
        # Set Box
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
        addProviderTiles(providers$Esri.WorldImagery)%>%
        #addLegend("bottomright", pal = loc.pal, values = ~dat.map.all$Release.location, title = "Release Location",opacity = 1)%>%# 
        # Inset Map
        addMiniMap(position = "topleft")%>% 
        # Measure tool
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479")%>%
        addScaleBar(position = "bottomleft")%>%
        # Legend
        addLegend("bottomleft", pal = loc.pal, values = ~points$Release.location, title = "Release Location",opacity = 1)
      
    } else if (input$base_map == "Open street map") {
      leaflet(points) %>% 
        addCircleMarkers(data=filter(points,Sex%in%input$Sex),~Longitude,~Latitude,radius = 4,color="black",fillColor = ~loc.pal(Release.location),stroke = TRUE,weight=1, fillOpacity = 0.5,popup = ~as.character(Labels))%>%
        # Set Box
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
        addTiles(group="Open street map")%>%
        #addLegend("bottomright", pal = loc.pal, values = ~dat.map.all$Release.location, title = "Release Location",opacity = 1)%>%# 
        # Inset Map
        addMiniMap(position = "topleft")%>% 
        # Measure tool
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479")%>%
        addScaleBar(position = "bottomleft")%>%
        # Legend
       addLegend("bottomleft", pal = loc.pal, values = ~points$Release.location, title = "Release Location",opacity = 1)
    }
  })
}

