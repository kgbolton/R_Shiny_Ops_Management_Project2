library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(rgeos)
library(tigris)
library(dplyr)
library(DT)

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
                    max(2779322),
                    value=c(min(crimes2018$Population),2779322)),
        radioButtons("crimeType",
                     "Type of crimes",
                     choices = c("Violent" = "Violent",
                                 "Property" = "Property",
                                 "All" = "X2018.Total.Index")),
        wellPanel("How many ways can you interpret a trend... Or lack thereof?")
    ),
    tabsetPanel(
        tabPanel("Map",
                 checkboxInput("showParks",
                               "Show mobile home parks",
                               value = T),
                 leafletOutput("FL")),
        tabPanel("Plots",
                 plotlyOutput("parkTrend"),
                 plotlyOutput("popTrend")),
        tabPanel("Data",
                 downloadButton("downloadData", "Download"),
                 DT::dataTableOutput("table"))
    )

)

# Define server function
server <- function(input, output) {
    # Basic Map
    output$FL <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
            setView(-81.5158, 27.6648, 6)
    })
    # Join and filter data
    allInputs <- reactive({
        # get park counts
        mobiles <- mobileHomes@data %>% group_by(COUNTY) %>% count()
        colnames(mobiles) <- c("COUNTYNAME", "Number of mobile home parks")
        mobiles$COUNTYNAME <- as.character(mobiles$COUNTYNAME)
        specialrow <- which(mobiles$COUNTYNAME == "DADE")
        mobiles[specialrow,1] <- "MIAMI-DADE"
        
        # join to county geospatial data
        countyParks <- geo_join(counties, mobiles, by = "COUNTYNAME")
        countyParks@data$Number.of.mobile.home.parks[is.na(countyParks@data$Number.of.mobile.home.parks)] <- 0
        # join crime data to county geospatial data
        countyAll <- geo_join(countyParks, crimes2018[,c(1:4, 12:15)], by = "COUNTYNAME")
        
        # filter data by population input
        countyAll <- subset(countyAll, Population>=input$popSize[1] & Population<=input$popSize[2])
        
        return(countyAll)
    })
    # mobile home parks
    observe({
        parks <- mobileHomes@data
        
        leafletProxy("FL", data = parks) %>% 
            addCircleMarkers(~LONG_DD, ~LAT_DD, radius = 2, group = "MHPs")
        
        if(!input$showParks) leafletProxy("FL", data = parks) %>% 
            clearGroup("MHPs")

    })
    # Color counties by crime rate and exclude based on population inputs
    observe({
        counties <- allInputs()
        # Create color gradient based on selected crime types
        pal <- colorBin(
            palette = hcl.colors(5, palette = "viridis", rev = T), 
            domain = counties[[input$crimeType]]/counties[["Population"]]*100, 
            bins = 5,
            pretty = F
        )
        # Augment map
        leafletProxy("FL", data = counties) %>%
            clearControls() %>%
            clearGroup("counties") %>%
            addPolygons(group = "counties", color = ~pal(counties[[input$crimeType]]/Population*100), label = ~COUNTYNAME) %>%
            addLegend("bottomleft", pal = pal, values = ~counties[[input$crimeType]]/Population*100,
                      title = "Crime rate (per 100 people)")
    })
    # A plot looking at how crime varies with the number of mobile home parks in a county
    output$parkTrend <- renderPlotly({
        countyAll <- allInputs()
        ggplot(data = countyAll@data, aes(x = countyAll[["Number.of.mobile.home.parks"]], y = countyAll[[input$crimeType]]/countyAll[["Population"]]*100)) + 
            geom_point() +
            theme_bw() + 
            labs(x = "Number of mobile home parks in the county", y = "Crime rate (per 100 people)", title = "Crime vs. mobile home parks")
    })
    # A plot looking at how crime varies with the population of a county
    output$popTrend <- renderPlotly({
        countyAll <- allInputs()
        ggplot(data = countyAll@data, aes(x = countyAll[["Population"]], y = countyAll[[input$crimeType]]/countyAll[["Population"]]*100)) + 
            geom_point() +
            theme_bw() + 
            labs(x = "Population of county", y = "Crime rate (per 100 people)", title = "Crime vs. population size")
    })
    # Display datatable
    output$table <- DT::renderDataTable(datatable(allInputs()@data[,c(4,11,9,12:17)], rownames = F) %>%
                                      formatStyle(columns = 1:9,
                                          backgroundColor = 'gray')
    )
    # Download handler
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(allInputs()@data[,c(4,11,9,12:17)], file)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
