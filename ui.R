library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

sex.vars<- c(
  "Female" = "Female",
  "Male" = "Male"
)

colour.vars<- c(
  "Red" = "Red",
  "White" = "White"
)

size.vars<- c(
  "Sub-legal" = "Sub-legal",
  "Legal" = "Legal",
  "Sub-legal to Legal" = "Sub-legal to Legal"
)

navbarPage("Recaptures", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Choose data to plot"),
                                      radioButtons("base_map", "Choose a base map:",c("Ocean basemap", "World imagery","Open street map")),
                                      selectInput("Sex", "Filter sex (can select both):", sex.vars,multiple=TRUE),
                                      selectInput("Colour", "Filter colour (can select both):", colour.vars,multiple=TRUE),
                                      selectInput("Size", "Filter size class (can select mutliple):", size.vars, multiple = TRUE),
                                      dateRangeInput('dateRange',label = 'Date range (yyyy-mm-dd):',
                                                     start = "2017-11-27", end = Sys.Date()),
                                      sliderInput("range", "Total distance travelled (km):",min = 0, max = 1000,value = c(25,100))),
                        
                        tags$div(id="cite",
                                 'Created by Brooke Gibbons, for UWA and DPIRD (2019).'
                        )
                    )
           )
)
