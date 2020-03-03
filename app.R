library(shiny)
library(shinydashboard)
library(shinyjs)
library(colourpicker)
library(shinycssloaders)
library(plotly)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(scales)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(RSocrata)
library(geojsonio)

# Function For Starting Date in UI (Outputs 1st of Month or Previous Month)
monthStart <- function(x) {
  if (day(as.POSIXlt(x)) < 15) {
    x <- as.POSIXlt(x)
    month(x) <- month(x)-1
    x$mday <- 1
    as.Date(x)
  }
  else{
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
  }
}

# Date Used for Leaflet Last 3 Months of Data
ct <- Sys.time()
month(ct) <- month(Sys.time())-3
ct<- as.Date(ct)

# Get API Key
token <- read.csv("token.csv", stringsAsFactors = FALSE)
token <- token$token[1]

# CSV of the Community Areas
comAreas <- read.csv("CommAreas.csv")
colnames(comAreas)[6] <- "community_area"
colnames(comAreas)[7] <- "community"

# Load Polygons
community <- geojson_read("CommAreas.geojson", what = "sp")

# Make the Community Areas Bold for Labels on Map
community$Bold <- paste0('<strong>', community$community, '</strong>') %>% 
  lapply(htmltools::HTML)

# CSS Here
CSS <- "#loading-content {
  position: absolute;
  background: #000000;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
  margin-top: 40px;
}

#map {
height: calc(100vh - 110px) !important;} 

.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'No observations returned for that criteria'; }"

# Specify Title Here so the "Strong" Tags Don't Appear in Tab
ui <- dashboardPage(skin = "black", title = "Chicago Crime",
  
    ## HEADER ##
    dashboardHeader(title = strong("Chicago Crime")),
    
    ## SIDEBAR ##
    dashboardSidebar(
      sidebarMenu(
        menuItem("Crime Map", tabName = "tab1", icon = icon("globe")),
        menuItem("Time Series/Table", tabName = "tab2", icon = icon("chart-line")),
        menuItem("GitHub", icon = icon("github"), href = "https://github.com/blg-uwm/ChicagoCrime", newtab = TRUE),
        menuItem("Data Source", icon = icon("table"), 
                 href = "https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2/data", newtab = TRUE)
      )
    ),
    
    
    ## BODY ##
    dashboardBody(
      useShinyjs(),
      inlineCSS(CSS),
      div(id = "loading-content",
          h2("Loading...")
      ),
        div(id = "app-content",
          tabItems(
            ## TAB 1 - Map ##
            tabItem(tabName = "tab1",
                    fillPage(
                          fluidRow(
                            column(width = 12, div(
                              style = "padding: 0px; margin: -15px;",
                              h3(strong("City of Chicago API Powered by Socrata"), style = "padding-left:15px; margin-top: 0px;"),
                              h5("Visualizing Homicide + the Top 10 Crime Types for the Past Three Months", style = "padding-left:15px;"),
                              withSpinner(leafletOutput("map", width = "100%", height = "100%"), type = 5, color = "#0e80c2", size = 2)
                            ))
                          )
                    )
        
            ),
            ## TAB 2 - Graph/Table ##
            tabItem(tabName = "tab2",
              
              ## ROW 1 ##
              fluidRow(
                column(width = 5,
                  h3(style = "margin-top: 0px;", strong("City of Chicago API Powered by Socrata")),
                  img(src = 'chicago.jpg', height = "10%", width = "85%")
                ),
                br(),
                br(),
                br(),
                br(),
                box(width = 4, status = "primary", align = 'center', height = 300,
                  selectInput("type", "Crime Type", c("Battery", "Assault", "Robbery", "Narcotics", "Theft", 
                              "Deceptive Practice", "Burglary", "Arson", "Other Offense", "Crim Sexual Assault", 
                              "Motor Vehicle Theft","Offense Involving Children", "Weapons Violation", 
                              "Interference With Public Officer","Concealed Carry License Violation", "Criminal Damage", 
                              "Criminal Trespass", "Gambling", "Homicide", "Human Trafficking", "Intimidation", 
                              "Kidnapping", "Liquor Law Violation", "Non-Criminal", "Obscenity", "Public Indecency",
                              "Public Peace Violation", "Sex Offense", "Stalking"), "Battery"),
                  
                  dateRangeInput("date", "Date", start = monthStart(Sys.Date()), end = Sys.Date()-8, min = "2014-01-01",
                                 max = Sys.Date()-8, separator = " to "),
                  h5("* Data trails by 8 days"),
                  actionButton("load", "Fetch Records", icon = icon("download"), style = 'height:60px; color: white; 
                               border-radius: 12px; background-color: #0e80c2; font-size: 16px;')
                ),
                column(width = 3,
                  h4(icon("info-circle"), "Set the parameters and click", em("Fetch Records"), "to render a time series graph and table of the crime"),
                  br(),
                  br(),
                  h5(em("Note:"), "Requesting more data will increase load times")
                )
              ),
              
              ## ROW 2 ##
              fluidRow(
                column(12,
                  # Output the Graph that Shows a Loading Symbol Before Rendered
                  withSpinner(plotlyOutput("graph", height = "275px"), type = 5, color = "#0e80c2", size = 2),
                  
                  # Output the Table that Shows a Loading Symbol Before Rendered
                  withSpinner(dataTableOutput("table"), type = 5, color = "#0e80c2", size = 2)
                )
              )
            )
          )
        )
    )
)


# Define Server
server <- function(input, output) {
  # Get API Data
  m <- read.socrata(paste0("https://data.cityofchicago.org/resource/ijzp-q8t2.json?$where=date > ", "'", ct, "'"), token)
  
  # Merge with Community Area Data
  m <- merge(m, comAreas[,c(6:7)], by = "community_area")
  
  # Clean Up
  m <- m %>% filter(!is.na(latitude))
  options(digits = 11)
  m$latitude <- as.numeric(m$latitude)
  m$longitude <- as.numeric(m$longitude)
  m$primary_type <- str_to_title(m$primary_type)
  m$primary_type <- as.factor(m$primary_type)
  m$description <- str_to_title(m$description)
  m$Lab <- paste('<strong>', m$primary_type, '</strong>', '<br/>', m$description, '<br/>', m$date) %>% 
    lapply(htmltools::HTML)
  
  # Top 10 Crime Types for Color Palette
  temp <- m %>% group_by(primary_type) %>% tally() %>% arrange(desc(n)) %>% top_n(10, n) %>% droplevels()
  pal <- colorFactor(palette = "Paired", levels = temp$primary_type)
  
  # Filtered Crime Types for Leaflet
  data1 <- m %>% filter(primary_type==levels(temp$primary_type)[1]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data2 <- m %>% filter(primary_type==levels(temp$primary_type)[2]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data3 <- m %>% filter(primary_type==levels(temp$primary_type)[3]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data4 <- m %>% filter(primary_type==levels(temp$primary_type)[4]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data5 <- m %>% filter(primary_type==levels(temp$primary_type)[5]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data6 <- m %>% filter(primary_type==levels(temp$primary_type)[6]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data7 <- m %>% filter(primary_type==levels(temp$primary_type)[7]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data8 <- m %>% filter(primary_type==levels(temp$primary_type)[8]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data9 <- m %>% filter(primary_type==levels(temp$primary_type)[9]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  data10 <- m %>% filter(primary_type==levels(temp$primary_type)[10]) %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  homicide <- m %>% filter(primary_type=='Homicide') %>% droplevels() %>% mutate(primary_type = as.character(primary_type))
  
  # Page 1 Graph
  output$map <- renderLeaflet({
               
  # Plot Leaflet Map
  leaflet(options = leafletOptions(minZoom = 10, preferCanvas = TRUE)) %>% 
  addProviderTiles("CartoDB.DarkMatter", group = "Dark Theme") %>%
  addProviderTiles("CartoDB.Positron", group = "Light Theme") %>%
  setView(lng = -87.654231, lat = 41.877562, zoom = 11) %>%
  setMaxBounds(lng1 = -87.455032, lat1 = 41.6, lng2 = -87.955673, lat2 = 42.040927) %>%
  addPolygons(data = community, color = "#0e80c2", fillColor = "white", label = ~Bold, 
              highlightOptions = highlightOptions(color = "#A9A9A9", opacity = 1, weight = 8, stroke = 8), 
              labelOptions = labelOptions(textsize = "13px")) %>%
  addCircleMarkers(data = data1, group = data1$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data2, group = data2$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data3, group = data3$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>%
  addCircleMarkers(data = data4, group = data4$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data5, group = data5$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data6, group = data6$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data7, group = data7$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data8, group = data8$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data9, group = data9$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addCircleMarkers(data = data10, group = data10$primary_type[1], radius = 2, color = ~pal(primary_type), label = ~Lab, 
                   labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>% 
  addPulseMarkers(data = homicide, group = "Homicide", label = ~Lab, icon = makePulseIcon(iconSize = 8), 
                  labelOptions = labelOptions(style = list("border" = "4px solid gray"), textsize = "12px")) %>%
  addLayersControl(overlayGroups = c("Homicide", data1$primary_type[1], data2$primary_type[1], data3$primary_type[1], data4$primary_type[1], 
                                     data5$primary_type[1], data6$primary_type[1], data7$primary_type[1], data8$primary_type[1], 
                                     data9$primary_type[1], data10$primary_type[1]), 
                   baseGroups = c("Dark Theme", "Light Theme")) %>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addLegend(pal = pal, values = temp$primary_type, title = "Crime Type", position = "bottomright") %>%
  addControl("<B>Map Filters Above</B>", position='topright')

})
 
  ## Page 2 Graph & Table ##
  
  # Main API Call
  crime2 <- eventReactive(input$load, {
    read.socrata(paste0("https://data.cityofchicago.org/resource/ijzp-q8t2.json?$where=date between ", 
                        "'", input$date[1], "T01:00:00", "'", " and ", "'", input$date[2]-1, "T23:00:00", "'", " AND primary_type = ", 
                        "'", str_to_upper(input$type), "'"), token) %>% 
      merge(comAreas[,c(6:7)], by = "community_area") %>% 
      transmute(Date = date, Block = str_to_title(block), District = district, Description = str_to_title(description), 
             Location_Description = str_to_title(location_description), Primary_Type = str_to_title(primary_type), 
             Arrest = arrest, Community = str_to_title(community), Latitude = latitude, Longitude = longitude) %>% arrange(Date) 
  })
  
  # Plotly Time Series Graph 
  output$graph <- renderPlotly({
    p <- crime2() %>% group_by(Date = as.Date(Date))  %>% summarise(N = dplyr::n()) %>%
      ggplot(aes(x = Date, y = N)) + geom_line() + ylab("Count") + xlab("Date") + scale_x_date(expand = c(0,0)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14))
    
    ggplotly(p)
  })
  
  # Crime Table
  output$table <- renderDataTable(crime2()[,-c(6)], extensions = c('Buttons', 'Scroller'), 
                                  options = list(dom = 'Bfrtip', deferRender = TRUE, scrollY = 400, scroller = TRUE, 
                                                 buttons = c('csv', 'excel')))
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
}

# Run the Application 
shinyApp(ui = ui, server = server)
