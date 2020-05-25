#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(zoo)
library(shinyWidgets)

setwd("C:/Users/deepa/Desktop/police-qgis/files")



month_ <- "April"
year_ <- "2018"


### Data prepared using python is being added as time series
crimes_data <- read.csv(file = "./data/police_filtered_data.csv")
tweets_data <- read.csv(file = "./data/tweet_sentiment_ym.csv")

 

## converting the data to a timeseries
crime_by_ym <- crimes_data %>% group_by(Year.Month)
crime_by_ym <- aggregate(crimes_data $Victimisations, by=list(crimes_data$Year.Month), FUN=sum)
names(crime_by_ym)[1] <- "monthYear"
names(crime_by_ym)[2] <- "crimeCount"
crime_by_ym$monthYear <- as.yearmon(crime_by_ym$monthYear,format="%B  %Y")

crime_count <- read.zoo(crime_by_ym)

#twitter data

names(tweets_data)[1] <- "monthYear"
tweets_data$monthYear <- as.yearmon(tweets_data$monthYear,format="%Y-%m")

tweets_data$all <- tweets_data$neg+tweets_data$pos+tweets_data$neu

pos_tweets_by_ym <- select(tweets_data,c("monthYear","pos")) 
neg_tweets_by_ym <- select(tweets_data,c("monthYear","neg")) 
neu_tweets_by_ym <- select(tweets_data,c("monthYear","neu")) 
all_tweets_by_ym <- select(tweets_data,c("monthYear","all")) 



positive_tweets <- read.zoo(pos_tweets_by_ym)
negative_tweets <- read.zoo(neg_tweets_by_ym)
neutral_tweets <- read.zoo(neu_tweets_by_ym)
all_tweets <- read.zoo(all_tweets_by_ym)


### setting up the data for map

pal <- colorBin("RdYlBu", domain = 0:39)
nz_regions <- readOGR("./data/vector2/vector.shp")
#labels <- paste("<p>",nz_regions$NAME_2,"</p>")
 



ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(
        sliderInput("year_range",label = "Year:",
                    min = 2015,
                    max = 2019,
                    value = 1,
                    sep = "",
                    step = 1),
 
        sliderTextInput(
          inputId = "month_range", 
          label = "Month:", 
          grid = TRUE, 
          force_edges = TRUE,
          choices = c("January","February","March","April","May","June",
                      "July","August","September","October","November","December"
                      )
        )
       
         
    ),
    dashboardBody(
        fluidRow(column(width=12,box(width=12,leafletOutput(  outputId = "victimizations_Map"))),
         
        ),
        fluidRow(
            dygraphOutput(outputId = "timeseriesplot")
        ),
        tags$head(tags$style(HTML(
            '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    ')),
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> NZ Police Choropleths </span>\');
      })
     '))
    )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
 
  labels <-reactive({
    paste("<p>",nz_regions$NAME_2,"</P>"
          ,"<p>",crimes_data[which(crimes_data$Year.Month == 
                                     paste(input$month_range,input$year_range)),]$Victimisations*10,
    "</p>",sep = "")
  })
  
    output$timeseriesplot <- renderDygraph({
        timeseriesData <- cbind(crime_count ,all_tweets,positive_tweets,
                                neutral_tweets,negative_tweets)#(ldeaths,mdeaths, fdeaths)
        dyRangeSelector(dygraph(timeseriesData, 
                                main = "#crime and #tweets@nzpolice")%>%
                          dyLegend(width = 600), 
                        dateWindow = c("2014-07-01", "2020-03-01"))
    }
    )

    output$victimizations_Map <- renderLeaflet(
        

      
       leaflet() %>% 
         #addProviderTiles(providers$Esri.WorldStreetMap) %>%
         addProviderTiles(providers$Stamen.TerrainBackground) %>%
         setView(lat = -40.9006, lng  = 175.8860, zoom =4.5) %>%
       addPolygons(data = nz_regions,
                    weight =1,
                   smoothFactor = 0.5,
                   color = "white",
                    fillOpacity = 0.8,
                   fillColor = pal(log10(crimes_data[which(crimes_data$Year.Month == 
                                                             paste(input$month_range,input$year_range)),]$Victimisations)*10),
                   highlightOptions = highlightOptions(
                     weight=1,
                     color = "green",
                      
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(),HTML)) %>% 
         addLegend(pal=pal,
                   values = crimes_data[which(crimes_data$Year.Month == 
                                                      paste(input$month_range,input$year_range)),]$Victimisations,
                   opacity = 0.7,
                   position="topright",
                   title = "Log scale")
       
        
            # leaflet() %>%
            # addProviderTiles(providers$Esri.WorldStreetMap) %>%
            # setView(lat = -40.9006, lng  = 175.8860, zoom =4.5) %>%
            # addPolygons(data= nz_regions,color = "#660000",weight=1)
        
    )
 
     
}

# Run the application 
shinyApp(ui = ui, server = server)
