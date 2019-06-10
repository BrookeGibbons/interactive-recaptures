# Load Libraries
library(rsconnect)
library(shiny)
library(dplyr)

# Set up account info to connect to shinyapps
rsconnect::setAccountInfo(name='brookegibbons',token='6D757FE28C61F12274F1E16C30783B95',
                          secret='zu/Mcxyo0ZN3qX/QTvc/SYoeRxuoY9LdPSDIET09') # connect to shinyapps

# Set working directory
setwd("C:/GitHub/interactive-recaptures")

# Testing
runApp() # test locally

# Saving
deployApp() # save to shinyapps
