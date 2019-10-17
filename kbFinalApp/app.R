library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(rgeos)

# Data sources:
# counties - http://geodata.myflorida.com/datasets/4abd0a3669204df2bc3a57066d217959_4
# mobile homes - http://geodata.myflorida.com/datasets/46e305880deb4138b1a110010da1c98d_0
# crimes - https://www.fdle.state.fl.us/FSAC/Data-Statistics/UCR-Offense-Data.aspx
counties <- readOGR("https://opendata.arcgis.com/datasets/4abd0a3669204df2bc3a57066d217959_4.geojson")
counties$COUNTYNAME <- as.character(counties$COUNTYNAME)
counties$COUNTYNAME[counties$COUNTYNAME=="DADE"] <- "MIAMI-DADE"
mobileHomes <- readOGR("https://opendata.arcgis.com/datasets/46e305880deb4138b1a110010da1c98d_0.geojson")
crimes2018 <- read.csv("offcty_index_2018.csv", stringsAsFactors = F)
colnames(crimes2018)[1] <- "COUNTYNAME"
crimes2018$COUNTYNAME <- toupper(crimes2018$COUNTYNAME)

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cyborg"),
    titlePanel("Florida Crime, and Mobile Homes"),
    flowLayout(
        sliderInput("popSize",
                    "Population of County",
                    min(crimes2018$Population),
                    max(crimes2018$Population),
                    value=c(min(crimes2018$Population),max(crimes2018$Population))),
        checkboxGroupInput("crimeType",
                           "Type of crimes",
                           choices = c("Violent", "Property")),
        wellPanel("How many ways can you interpret a trend... Or lack thereof?")
    ),
    tabsetPanel(
        tabPanel("Map",
                 checkboxInput("showParks",
                               "Show mobile home parks",
                               value = F),
                 leafletOutput("FL")),
        tabPanel("Plots"),
        tabPanel("Data")
    )

)

# Define server function
server <- function(input, output) {
    # Basic Map
    output$FL <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap.BlackAndWhite)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
