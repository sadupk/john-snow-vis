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
library(ggrepel)

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
cholera_age_sex$age = factor(cholera_age_sex$age, levels = cholera_age_sex$age)

#New columns cumsum for cholera_deaths
cholera_deaths$Attack_cum = cumsum(cholera_deaths$Attack)
cholera_deaths$Death_cum = cumsum(cholera_deaths$Death)

#New column and names for UK census
UK_census$age = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "over 80")
UK_census[c("male", "female")]=round(UK_census[c("male", "female")], digits = -4)
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

#Data prep for pyramids
cholera_age_sex_pyramid = cholera_age_sex 
cholera_age_sex_pyramid$female = -cholera_age_sex_pyramid$female
cholera_age_sex_pyramid = melt(cholera_age_sex_pyramid, id.vars = "age")
UK_census_pyramid = UK_census[c("age","male", "female")]
UK_census_pyramid$female = -UK_census_pyramid$female
UK_census_pyramid$male = UK_census_pyramid$male/UK_census_sum$total[1]*100
UK_census_pyramid$female = UK_census_pyramid$female/UK_census_sum$total[2]*100
UK_census_pyramid = melt(UK_census_pyramid, id.vars = "age")
lbls = paste0(as.character(c(seq(40, 0, -5), seq(5, 40, 5))))

#Plot JPEG and hover variables
df = data.frame(x = 1:100, y = 1:100)
map = jpeg::readJPEG("18p1data/snowMapRobinWilson.jpg")

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

#Plot theme from http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


ui = dashboardPage(skin = "red",
  dashboardHeader(title = "John Snow's Dashboard",
                  titleWidth = 400
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("fas fa-info")),
      menuItem("The overall picture", tabName = "overall", icon = icon("bar-chart-o")),
      menuItem("Who was attacked", tabName = "who", icon = icon("bar-chart-o")),
      menuItem("The 1851 UK Census", tabName = "census", icon = icon("th")),
      menuItem("John Snow's Map", tabName = "map_snow", icon = icon("fas fa-map")),
      menuItem("Modern London Map", tabName = "map_modern", icon = icon("fas fa-map"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
        font-weight: bold;
        font-size: 24px;
      }
      .main-sidebar {
        font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
        font-weight: bold;
        font-size: 14px;
      }
      h1 {
      	font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      	font-size: 24px;
      	font-style: normal;
      	font-variant: normal;
      	font-weight: 500;
      	line-height: 26.4px;
      }
      h3 {
      	font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      	font-size: 14px;
      	font-style: normal;
      	font-variant: normal;
      	font-weight: 500;
      	line-height: 15.4px;
      }
      .content-wrapper {
        background-color: white !important;
      }
    '))),
    tabItems(
      tabItem(tabName = "about",
                h1("About this project:"), 
                h3("Hello, /n this project was done by Pedro Borges for CS424. The goal is to explore 
                   the data collected by John snow in 1851 and present it in a modern way to draw similar conclusion to his.
                   All of the data used to produce this project can be downloaded at GITHUB LINK..")
      ),
      tabItem(tabName = "overall",
              fluidRow(
                title = "Cholera Deaths and Attacks", plotOutput("plot0")
              ),
              fluidRow(
                dataTableOutput(
                  "table0"
                )
              )
      ),
      tabItem(tabName = "who",
              fluidRow(
                column(6,
                       title = "Woman Attacked", plotOutput("plot2")
                ),
                column(6,
                       title = "Men Attacked", plotOutput("plot3")
                )
              ),
              fluidRow(
                tabBox(
                  title = "The Population Distribution in Numbers",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "table1",
                  tabPanel("Cholera Deaths", dataTableOutput("table1")),
                  tabPanel("UK 1852 Census", dataTableOutput("table2"))
                )
              )
      ),
      tabItem(tabName = "census",
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
      )),
      tabItem(tabName = "map_modern",
              fluidRow(
                leafletOutput("leaf")
              )),
      tabItem(tabName = "map_snow",
              fixedRow(
                column(width = 2,
                       verbatimTextOutput("hover_info")
                )
              ),
              fluidRow(
                
                column(width = 6,
                       plotOutput("jpeg", height = "auto",hover = hoverOpts(id ="plot_hover")),
                       uiOutput("dynamic")
                )
              )
      )
    )
  ))


server = function(input, output, session) {
  output$plot0 = renderPlot({
    ggplot(cholera_deaths_long, aes(x=Date, y=value, color = variable)) + geom_line(size = 2) + 
      theme(legend.title=element_blank()) +
      scale_color_manual(values=wes_palette(n=4, name="GrandBudapest2"), 
                         labels=c("Attacks", "Deaths", "Cummulatice Attacks", "Cummulative Deaths")) +
    theme_bw()
  })
  output$plot1 = renderPlot({
    ggplot(cholera_age_sex_long, aes(x=age, y=value,fill=factor(variable))) + geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = wes_palette(n=2, name="GrandBudapest"))
  })
  output$plot2 = renderPlot({
    ggplot(cholera_age_sex_pyramid, aes(x = age, y = value, fill = variable)) + 
      geom_bar(stat = "identity", width = .6) +
      scale_y_continuous(breaks = seq(-40, 40, 5), limits = c(-40,40), labels = lbls) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") + 
      theme_bw() +
      labs(title="Cholera Outbreak Death by Age Group", y = "Deaths per 1000 People", x = "Age Group")
  })
  output$plot3 = renderPlot({
    ggplot(UK_census_pyramid, aes(x = age, y = value, fill = variable)) + 
      geom_bar(stat = "identity", width = .6) +
      scale_y_continuous(breaks = seq(-40, 40, 5), limits = c(-40,40), labels = lbls) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") + 
      theme_bw() +
      labs(title="UK 1851 Population Distribution", y = "% of Total Population", x = "Age Group")
  })
  output$plot4 = renderPlot({
    ggplot(UK_census, aes(1, male, fill = age)) +
      geom_col(color = 'black', 
               position = position_stack(reverse = TRUE), 
               show.legend = FALSE) +
      geom_text_repel(aes(x = 1.4, y = (cumsum(c(0, male)) + c(male / 2, .01))[1:nrow(UK_census)], label = age), 
                      nudge_x = .3, 
                      segment.size = .7, 
                      show.legend = FALSE) +
      coord_polar('y') +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$plot5 = renderPlot({
    ggplot(UK_census, aes(1, female, fill = age)) +
      geom_col(color = 'black', 
               position = position_stack(reverse = TRUE), 
               show.legend = FALSE) +
      geom_text_repel(aes(x = 1.4, y = (cumsum(c(0, female)) + c(female / 2, .01))[1:nrow(UK_census)], label = age), 
                      nudge_x = .3, 
                      segment.size = .7, 
                      show.legend = FALSE) +
      coord_polar('y') +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$plot6 = renderPlot({
    ggplot(UK_census, aes(x=age, y=male)) + geom_bar(stat = "identity")
  })
  output$plot7 = renderPlot({
    ggplot(UK_census, aes(x=age, y=female)) + geom_bar(stat = "identity")
  })
  output$plot8 = renderPlot({
    ggplot(UK_census_sum, aes(x="", y=total, fill=group)) + 
      geom_bar(width = 1, stat = "identity", color = "black") + 
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Pastel1") +
      theme_void() +
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = total/2 + c(0, cumsum(total)[-length(total)]), 
                    label = format(total, big.mark = ",")), size=5) +
      labs(fill = "")
  })
  output$table0 = renderDataTable({
    datatable(cholera_deaths, rownames = FALSE,  
              colnames = c('Date', 'Daily Attacks', 'Daily Deaths', 'Cummulative Attacks', 'Cummulative Deaths')
              ) %>%
      formatStyle(names(cholera_deaths), backgroundColor = "white")
  })
  output$table1 = renderDataTable({
    datatable(cholera_age_sex, rownames = FALSE,  
              colnames = c('Age', 'Male deaths per 1000', 'Female deaths per 1000'),
              options = list(dom = 't')
    ) %>%
      formatStyle(names(cholera_age_sex), backgroundColor = "white")
  })
  output$table2 = renderDataTable({
    datatable(UK_census, rownames = FALSE,  
              colnames = c("Age", "Number of Males", "Number of Females", "Total"),
              options = list(dom = "t")
    ) %>%
      formatStyle(names(UK_census), backgroundColor = "white") %>%
      formatRound(c("male", "female", "total"), interval = 3, mark = ",", digits = 0)
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
      geom_point(data = pump_locations_xy, aes(x = coords.x1, y = coords.x2, colour = ""), size = 8) +
      geom_point(data = cholera_death_locations_xy, aes(x = coords.x1, y = coords.x2, size = `cord$deaths`),
                 fill = "red", alpha =0.8, shape = 21) +
      scale_size_continuous(range = c(3, 15), name = "Deaths") +
      scale_colour_manual(values = "blue", name = "Pumps")
  }, 
  #Dynamic height code idea by jcheng5 https://github.com/rstudio/shiny/issues/650
  height = function() {
    session$clientData$output_jpeg_width
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