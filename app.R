library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape)
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

leaflet(data = cholera_death_locations) %>% addTiles(group = "Deaths") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
  addMarkers(clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE, animationOptions())) %>% 
  addMarkers(data = pump_locations, group = "Pumps", icon = pump_icon) %>%
  addLayersControl(
    baseGroups = c("Basemap", "Deaths"),
    overlayGroups = c("Pumps"),
    options = layersControlOptions(collapsed = TRUE)
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
 # leaflet(data = pump_locations) %>% addTiles() %>% +addMarkers(~long, ~lat)
}

shinyApp(ui <- ui, server = server)