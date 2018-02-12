library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(reshape)
library(rgdal)
library(RColorBrewer)
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
lbls = paste0(as.character(c(seq(40, 0, -10), seq(10, 40, 10))))

#Plot JPEG and hover variables
df = data.frame(x = 1:100, y = 1:100)
map = jpeg::readJPEG("18p1data/snowMapRobinWilson.jpg")

#Coordinates for JPEG projection plot
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
pal_cum = c("orangered",
            "slateblue",
            "orangered3",
            "slateblue4")

#For leaflet
pal_leaflet = colorNumeric(
  palette = pal_reds[5:9],
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
      menuItem("An epidemics progress", tabName = "overall", icon = icon("bar-chart-o")),
      menuItem("Who was affected", tabName = "who", icon = icon("bar-chart-o")),
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
      .nav-tabs {
      	font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      	font-size: 24px;
      	font-style: normal;
      	font-variant: normal;
      	font-weight: 500;
      	line-height: 26.4px;
      }
      .x-toolbar-title {
      	font-family: "Helvetica Neue", Helvetica, Arial, sans-serif !important;
      	font-size: 24px;
      	font-style: normal;
      	font-variant: normal;
      	font-weight: 500;
      	line-height: 26.4px;
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
      	font-size: 24px;
      	font-style: normal;
      	font-variant: normal;
      	font-weight: 500;
      	line-height: 26.4px;
      }
      text1 {
      	font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      	font-size: 25px;
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
                   the data collected by John snow in 1854 and present it in a modern way to draw similar conclusion to his.
                   All of the data used to produce this project can be downloaded at GITHUB LINK.."),
              h3("In the middle 1800s, people didn’t have running water or modern toilets in their homes. They used town 
                  wells and communal pumps to get the water they used for drinking, cooking and washing.  Septic systems 
                  were primitive and most homes and businesses dumped untreated sewage and animal waste directly into the 
                  Thames River or into open pits called “cesspools”.  Water companies often bottled water from the Thames 
                  and delivered it to pubs, breweries and other businesses."), 
              h3("Dr. Snow believed sewage dumped into the river 
                  or into cesspools near town wells could contaminate the water supply, leading to a rapid spread of disease.")
      ),
      tabItem(
        tabName = "overall",
        fluidRow(
          column(3,
                 h3("British doctor John Snow couldn’t convince other doctors and scientists that cholera, a deadly disease, 
                    was spread when people drank contaminated water until a mother washed her baby’s diaper in a town well 
                    in 1854 and touched off an epidemic that killed 616 people.")
                 ),
          column(8,
                 offest = 1,
          title = "Cholera Deaths and Attacks", plotOutput("plot0")
          )
        ),
        fluidRow(
          column(3,
                 h3("“Within 250 yards of the spot where Cambridge Street joins Broad Street there were upwards of 500 
                    fatal attacks of cholera in 10 days,” Dr. Snow wrote  “As soon as I became acquainted with the 
                    situation and extent of this irruption (sic) of cholera, I suspected some contamination of the 
                    water of the much-frequented street-pump in Broad Street.”")
          ),
          column(8,
                 offset = 1,
            dataTableOutput("table0")
          )
        )
      ),
      tabItem(tabName = "who",
              fluidRow(
                column(5,
                       title = "Woman Attacked", plotOutput("plot2")
                ),
                column(5,
                       offset = 1,
                       title = "Men Attacked", plotOutput("plot3")
                )
              ),
              fluidRow(
                column(5,
                       tabBox(
                         width = 12,
                         title = "The Population Distribution in Numbers",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "tabset2",
                         tabPanel("Cholera Deaths", dataTableOutput("table1")),
                         tabPanel("UK 1852 Census", dataTableOutput("table2"))
                       )
                ),
                column(5,
                       offset = 1,
                       tabBox(
                         width = 12,
                         title = "UK Census by sex and age group (range in years)",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "tabset2",
                         tabPanel("Total population", plotOutput("plot8")),
                         tabPanel("Male population", plotOutput("plot4")),
                         tabPanel("Female population", plotOutput("plot5"))
                       )
                )
              )
      ),
      tabItem(tabName = "map_modern",
              tags$style(type = "text/css", ".box-body {height:80vh}"),
              leafletOutput("leaf", width = "100%", height = 900)
      ),
      tabItem(tabName = "map_snow",
              class = "h3",
              fluidRow(
                column(width = 7,
                       plotOutput("jpeg", height = "auto",hover = hoverOpts(id ="plot_hover"))
                ),
                column(width = 5,
                       box(h1("Hover mouse over the death locations"),
                           width = 12,
                           height = 2,
                           uiOutput("hover_info")
                       )
                )
              )
      )
    )
  )
)


server = function(input, output, session) {
  output$plot0 = renderPlot({
    ggplot(cholera_deaths_long, aes(x=Date, y=value, color = variable)) + geom_line(size = 4) + 
      scale_color_manual(values=pal_cum, labels=c("Daily Attacks", "Daily Deaths", "Cummulative Attacks", "Cummulative Deaths")) +
      theme_bw() +
      theme(text = element_text(size=30)) +
      theme(legend.title=element_blank()) +
      scale_x_date(date_breaks = "5 day", date_labels = "%b %d") +
      labs(title="Cholera Deaths and Attacks", subtitle = "1854 epidemic",y = "Number of People", x = "Age Group")
  })
  #Line plot of attacks and deaths over time
  output$plot1 = renderPlot({
    ggplot(cholera_age_sex_long, aes(x=age, y=value,fill=factor(variable))) + geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = wes_palette(n=2, name="GrandBudapest"))
  })
  #Demographic pyramid of Naples Cholera attacks
  output$plot2 = renderPlot({
    ggplot(cholera_age_sex_pyramid, aes(x = age, y = value, fill = variable)) + 
      geom_bar(stat = "identity", width = .6) +
      scale_y_continuous(breaks = seq(-40, 40, 10), limits = c(-40,40), labels = lbls) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1", direction = -1) + 
      theme_bw() +
      labs(title="Cholera Death Demographics", subtitle = "Naples 1884-1911 epidemic", y = "Deaths per 1000 People", x = "Age Group") +
      theme(text = element_text(size=30)) +
      theme(legend.title=element_blank())
  })
  #Demographic pyramid of UK census
  output$plot3 = renderPlot({9
    ggplot(UK_census_pyramid, aes(x = age, y = value, fill = variable)) + 
      geom_bar(stat = "identity", width = .6) +
      scale_y_continuous(breaks = seq(-40, 40, 10), limits = c(-40,40), labels = lbls) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1", direction = -1) + 
      theme_bw() +
      labs(title="UK Demographics", subtitle = "1851 census",y = "% of Total Population", x = "Age Group") +
      theme(text = element_text(size=30)) +
      theme(legend.title=element_blank())
  })
  #Man demographic pie chart
  output$plot4 = renderPlot({
    ggplot(UK_census, aes(1, male, fill = age)) +
      geom_col(color = 'black', 
               position = position_stack(reverse = TRUE), 
               show.legend = FALSE) +
      geom_text_repel(aes(x = 1.4, y = (cumsum(c(0, male)) + c(male / 2, .01))[1:nrow(UK_census)], label = age), 
                      nudge_x = .2, 
                      segment.size = .7, 
                      show.legend = FALSE,
                      size = 8) +
      coord_polar('y') +
      theme_void() +
      scale_fill_brewer(palette="Blues")
  })
  #Female demographic pie chart
  output$plot5 = renderPlot({
    ggplot(UK_census, aes(1, female, fill = age)) +
      geom_col(color = 'black', 
               position = position_stack(reverse = TRUE), 
               show.legend = FALSE) +
      geom_text_repel(aes(x = 1.4, y = (cumsum(c(0, female)) + c(female / 2, .01))[1:nrow(UK_census)], label = age), 
                      nudge_x = .2,
                      segment.size = 1, 
                      show.legend = FALSE,
                      size = 8) +
      coord_polar('y') +
      theme_void() +
      scale_fill_brewer(palette="Reds") 
  })
  #Overall male and female pie chart
  output$plot8 = renderPlot({
    ggplot(UK_census_sum, aes(x="", y=total, fill=group)) + 
      geom_bar(width = 1, stat = "identity", color = "black") + 
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="Pastel1") +
      theme_void() +
      theme(axis.text.x=element_blank(),  text = element_text(size=30)) +
      geom_text(aes(y = total/2 + c(0, cumsum(total)[-length(total)]), 
                    label = format(total, big.mark = ",")), size=8) +
      labs(fill = "")
  })
  #Daily afflicted table
  output$table0 = renderDataTable({
    datatable(cholera_deaths, rownames = FALSE,  
              colnames = c('Date', 'Daily Attacks', 'Daily Deaths', 'Cummulative Attacks', 'Cummulative Deaths'),
              options = list(
                initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '25px'});}"))
    ) %>%
      formatStyle(names(cholera_deaths), backgroundColor = "white", `font-size` = '25px')
  })
  #Naples afflicted table
  output$table1 = renderDataTable({
    datatable(cholera_age_sex, rownames = FALSE,  
              colnames = c('Age', 'Male deaths per 1000', 'Female deaths per 1000'),
              options = list(dom = 't',
                             initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '25px'});}")
                             )
    ) %>%
      formatStyle(names(cholera_age_sex), backgroundColor = "white", `font-size` = '25px')
  })
  #UK census table
  output$table2 = renderDataTable({
    datatable(UK_census, rownames = FALSE,  
              colnames = c("Age", "Number of Males", "Number of Females", "Total"),
              options = list(dom = "t",
                             initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '25px'});}")
                             )
    ) %>%
      formatStyle(names(UK_census), backgroundColor = "white", `font-size` = '25px') %>%
      formatRound(c("male", "female", "total"), interval = 3, mark = ",", digits = 0)
  })
  #leaflet map
  output$leaf <- renderLeaflet({
    map =leaflet(data = cholera_death_locations) %>% addTiles(group = "Add City Information") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
      addCircleMarkers(radius = cholera_death_locations$deaths, color = pal_leaflet(cholera_death_locations$deaths), fillOpacity = 0.9) %>%
      addCircleMarkers(data = pump_locations, group = "Pumps", color = "Blue") %>%
      addLayersControl(
        baseGroups = c("Basemap", "Add City Information"),
        overlayGroups = c("Pumps"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(pal = pal_leaflet, values = cholera_death_locations$deaths,
                title = "Number of Deaths",
                opacity = 1
      ) %>%
      addLegend(colors= "Blue", labels="", title="Pumps")
  })
  #JPEG map
  output$jpeg = renderPlot({
    ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") + 
      annotation_custom(rasterGrob(map, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
      geom_point(data = pump_locations_xy, aes(x = coords.x1, y = coords.x2, colour = ""), size = 8) +
      geom_point(data = cholera_death_locations_xy, aes(x = coords.x1, y = coords.x2, size = `cord$deaths`),
                 fill = "red", alpha =0.6, shape = 21) +
      scale_size_continuous(range = c(3, 15), name = "Deaths") +
      scale_colour_manual(values = "blue", name = "Pumps") +
      theme_void() +
      theme(text = element_text(size=30))
  }, 
  #Dynamic height code idea by jcheng5 https://github.com/rstudio/shiny/issues/650
  height = function() {
    session$clientData$output_jpeg_width
  })
  output$hover_info = renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x - cholera_death_locations_xy$coords.x1)^2+(hover$y - cholera_death_locations_xy$coords.x2)^2)
      cat("Number of Dead at That Coordinate: ")
      if(min(dist) < 3)
        cat(cholera_death_locations_xy[,1][which.min(dist)])
    }
  })
}

shinyApp(ui <- ui, server = server)