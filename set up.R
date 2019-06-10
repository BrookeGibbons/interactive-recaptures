#install.packages('rsconnect')
library(rsconnect)
library(shiny)
library(dplyr)

rsconnect::setAccountInfo(name='brookegibbons',
                          token='6D757FE28C61F12274F1E16C30783B95',
                          secret='zu/Mcxyo0ZN3qX/QTvc/SYoeRxuoY9LdPSDIET09')


setwd("C:/GitHub/Interactive recaptures")

runApp() # test locally

deployApp()
