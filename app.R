library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape)
library(rgdal)
library(RColorBrewer)
library(wesanderson)

#Lots of code snippets here are inspired by Andy Johnson's demos in CS424

# Read the data in tsv format and format bad row
cholera_deaths = read.delim("18p1data/choleraDeaths.tsv", skip = 1, col.names = c("Date", "Attack", "Death"), header = FALSE)
cholera_age_sex = read.delim("18p1data/naplesCholeraAgeSexData.tsv", skip = 5, col.names = c("age", "male", "female"), header = FALSE)
UK_census = read.delim("18p1data/UKcensus1851.csv", skip = 2, sep = ",")
cholera_death_locations = read.delim("18p1data/choleraDeathLocations.csv", sep = ",", col.names = c("deaths", "long", "lat"), header = FALSE)
pump_locations = read.delim("18p1data/choleraPumpLocations.csv", sep = ",", col.names = c("long", "lat"), header = FALSE)

#Fix cholera_deaths
cholera_deaths$Date = as.character(cholera_deaths$Date)
bad_row = strsplit(cholera_deaths$Date[1], " ")
cholera_deaths$Date[1] = bad_row[[1]][1]
cholera_deaths$Death[1] = cholera_deaths$Attack[1]
cholera_deaths$Attack[1] = bad_row[[1]][2]

#Fix cholera_age_sex
cholera_age_sex$age = as.character(cholera_age_sex$age)
bad_rows = strsplit(cholera_age_sex$age[1:2], " ")
cholera_age_sex$female[1:2] = cholera_age_sex$male[1:2]
cholera_age_sex$male[1] = bad_rows[[1]][2]
cholera_age_sex$male[2] = bad_rows[[2]][2]
cholera_age_sex$age[1] = bad_rows[[1]][1]
cholera_age_sex$age[2] = bad_rows[[2]][1]

# Covert columns to appropriate data types
cholera_deaths$Date = as.Date(cholera_deaths$Date, format = "%d-%B-%Y")
cholera_deaths$Attack = as.integer(cholera_deaths$Attack)
cholera_deaths$Death = as.integer(cholera_deaths$Death)
cholera_age_sex$male = as.numeric(cholera_age_sex$male)
cholera_age_sex$female = as.numeric(cholera_age_sex$female)

#New columns cumsum for cholera_deaths
cholera_deaths$Attack_cum = cumsum(cholera_deaths$Attack)
cholera_deaths$Death_cum = cumsum(cholera_deaths$Death)

#New column for UK census
UK_census$total = UK_census$male + UK_census$female

#Line plot melted data for cholera_deaths
cholera_deaths_long = melt(cholera_deaths, id="Date")

#Bar plot of deaths by sex/age melted data for cholera_age_sex
cholera_age_sex_long = melt(cholera_age_sex, id.vars = "age")

#Pie chart of UK_census sum
UK_census_sum = data.frame(
  group = c("male", "female"),
  total = c(sum(UK_census$male), sum(UK_census$female))
)

#Custom leaflet pump marker
pump_icon <- makeIcon(
  iconUrl = "18p1data/drop-icon.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "18p1data/drop-icon.png",
  shadowWidth = 20, shadowHeight = 20,
  shadowAnchorX = 4, shadowAnchorY = 62
)

#Plot JPEG and hover variables
df = data.frame(x = 1:100, y = 1:100)
map = jpeg::readJPEG("~/Documents/CS424-Visualization/john-snow-vis/18p1data/snowMapRobinWilson.jpg")

#Coordinates for JPEG projection plot
# bottom_left = c(0, -0.143777, 51.509523)
bottom_left = c(0, -0.143755, 51.509440)

top_right = c(0, -0.131110, 51.516522)
cord = rbind(bottom_left, top_right, cholera_death_locations)
cord.dec = SpatialPoints(cbind(cord$long, cord$lat), proj4string=CRS("+proj=longlat"))
cord.UTM = spTransform(cord.dec, CRS("+init=epsg:27700"))
cord =cbind(cord$deaths,as.data.frame(cord.UTM))
cord_pumps = rbind(bottom_left[2:3], top_right[2:3], pump_locations)
cord.dec_pumps = SpatialPoints(cbind(cord_pumps$long, cord_pumps$lat), proj4string=CRS("+proj=longlat"))
cord.UTM_pumps = spTransform(cord.dec_pumps, CRS("+init=epsg:27700"))
cord.UTM_pumps = as.data.frame(cord.UTM_pumps)

#Scale projection to JPEG coordinates
cord$coords.x1 = (cord$coords.x1 - min(cord$coords.x1)) / (max(cord$coords.x1) - min(cord$coords.x1)) * nrow(df)
cord$coords.x2 = (cord$coords.x2 - min(cord$coords.x2)) / (max(cord$coords.x2) - min(cord$coords.x2)) * nrow(df)
cholera_death_locations_xy = cord[-c(1:2),]
cord.UTM_pumps$coords.x1 = (cord.UTM_pumps$coords.x1 - min(cord.UTM_pumps$coords.x1)) / (max(cord.UTM_pumps$coords.x1) - min(cord.UTM_pumps$coords.x1)) * nrow(df)
cord.UTM_pumps$coords.x2 = (cord.UTM_pumps$coords.x2 - min(cord.UTM_pumps$coords.x2)) / (max(cord.UTM_pumps$coords.x2) - min(cord.UTM_pumps$coords.x2)) * nrow(df)
pump_locations_xy = cord.UTM_pumps[-c(1:2),]

#Colors
pal_reds = brewer.pal(9,"OrRd")
#For leaflet
pal_leaflet = colorNumeric(
  palette = pal_reds[3:9],
  domain = cholera_death_locations$deaths
)

ui = dashboardPage(
  dashboardHeader(title = "John Snow's Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Cholera Deaths and Attacks", plotOutput("plot0")
          )
        ),
        fluidRow(
          dataTableOutput(
            "table0"
          )
        ),
        fluidRow(
          dataTableOutput(
            "table1"
          )
        ),
        fluidRow(
          box(
            title = "Woman Attacked", plotOutput("plot2")
          )
        ),
        fluidRow(
          box(
            title = "Men Attacked", plotOutput("plot3")
          )
        ),
        fluidRow(
          dataTableOutput(
            "table2"
          )
        ),
        fluidRow(
          box(
            title = "UK Census - Males", plotOutput("plot4")
          )
        ),
        fluidRow(
          box(
            title = "UK Census - females", plotOutput("plot5")
          )
        ),
        fluidRow(
          box(
            title = "UK Census - males", plotOutput("plot6")
          )
        ),
        fluidRow(
          box(
            title = "UK Census - females", plotOutput("plot7")
          )
        ),
        fluidRow(
          box(
            title = "UK Census - total by sex", plotOutput("plot8")
          )
        ),
        fluidRow(
          box(
            title = "Leaflet Map", leafletOutput("leaf")
          )
        ),
        fluidRow(
          column(width = 5,
                 verbatimTextOutput("hover_info")
          )
        ),
        fluidRow(
          
          column(width = 12,
                 plotOutput("jpeg", height = 1600,hover = hoverOpts(id ="plot_hover")),
                 uiOutput("dynamic")
          )
        )
      )
    )
))


server = function(input, output) {
  output$plot0 = renderPlot({
    ggplot(cholera_deaths_long, aes(x=Date, y=value, color = variable)) + geom_line(size = 2) + 
      theme(legend.title=element_blank()) +
      scale_color_manual(values=wes_palette(n=4, name="GrandBudapest2"), 
                         labels=c("Attacks", "Deaths", "Cummulatice Attacks", "Cummulative Deaths")) +
      theme_dark(20)
  })
  output$plot1 = renderPlot({
    ggplot(cholera_age_sex_long, aes(x=age, y=value,fill=factor(variable))) + geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = wes_palette(n=2, name="GrandBudapest"))
  })
  output$plot2 = renderPlot({
    ggplot(cholera_age_sex, aes(x=age, y=female)) + geom_bar(stat = "identity")
  })
  output$plot3 = renderPlot({
    ggplot(cholera_age_sex, aes(x=age, y=male)) + geom_bar(stat = "identity")
  })
  output$plot4 = renderPlot({
    ggplot(UK_census, aes(x="", y=male, fill=age)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
  })
  output$plot5 = renderPlot({
    ggplot(UK_census, aes(x="", y=female, fill=age)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
  })
  output$plot6 = renderPlot({
    ggplot(UK_census, aes(x=age, y=male)) + geom_bar(stat = "identity")
  })
  output$plot7 = renderPlot({
    ggplot(UK_census, aes(x=age, y=female)) + geom_bar(stat = "identity")
  })
  output$plot8 = renderPlot({
    ggplot(UK_census_sum, aes(x="", y=total, fill=group)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
  })
  output$table0 = renderDataTable({
    cholera_deaths
  })
  output$table1 = renderDataTable({
    cholera_age_sex
  })
  output$table2 = renderDataTable({
    UK_census
  })
  output$leaf <- renderLeaflet({
    map =leaflet(data = cholera_death_locations) %>% addTiles(group = "Deaths") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addCircleMarkers(radius = cholera_death_locations$deaths, color = pal_leaflet(cholera_death_locations$deaths), fillOpacity = 0.9) %>%
      addCircleMarkers(data = pump_locations, group = "Pumps", color = "Blue") %>%
      addLayersControl(
        baseGroups = c("Basemap", "Deaths"),
        overlayGroups = c("Pumps"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(pal = pal_leaflet, values = cholera_death_locations$deaths,
                title = "Number of Deaths",
                opacity = 1
      ) %>%
      addLegend(colors= "Blue", labels="", title="Pumps")
  })
  output$jpeg = renderPlot({
    ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") + 
      annotation_custom(rasterGrob(map, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
      geom_point(data = pump_locations_xy, aes(x = coords.x1, y = coords.x2, colour = "", size = 5)) +
      geom_point(data = cholera_death_locations_xy, aes(x = coords.x1, y = coords.x2, size = `cord$deaths`),
                 fill = "red", alpha =0.8, shape = 21) +
      scale_size_continuous(range = c(3, 15), name = "Deaths") +
      scale_colour_manual(values = "blue", name = "Pumps")
  })
  output$hover_info = renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x - cholera_death_locations_xy$coords.x1)^2+(hover$y - cholera_death_locations_xy$coords.x2)^2)
      cat("Number of Dead\n")
      if(min(dist) < 3)
        cholera_death_locations_xy[,1][which.min(dist)]
    }
  })
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })
}

shinyApp(ui <- ui, server = server)