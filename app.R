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

#New column Attack+Death Total
cholera_deaths$Total = cholera_deaths$Attack + cholera_deaths$Death
cholera_deaths$Cummulative_Total = cumsum(cholera_deaths$Total)

ui <- fluidPage("Hello World")

server <- function(input, output) {}

shinyApp(ui <- ui, server = server)

#Line plot
cholera_deaths_long = melt(cholera_deaths, id="Date")
ggplot(cholera_deaths_long, aes(x=Date, y=value, color = variable)) + geom_line(size = 2) + 
  scale_color_manual(values=wes_palette(n=4, name="GrandBudapest2"))

ui = dashboardPage(
  dashboardHeader(title = "John Snow's Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Noon Data as Table", solidHeader = TRUE, status = "primary", width = 4,
          dataTableOutput("tab1")
      )
    ),
    fluidRow(
      box( title = "Cholera Deaths and Attacks", solidHeader = TRUE, status = "primary", width = 12,
           plotOutput("plot0")
      )
    )
))
server = function(input, output) {
  output$plot0 = renderPlot({
    ggplot(cholera_deaths_long, aes(x=Date, y=value, color = variable)) + geom_line(size = 2) + 
      theme(legend.title=element_blank()) +
      scale_color_manual(values=wes_palette(n=4, name="GrandBudapest2"), 
                         labels=c("Attacks", "Deaths", "Total Attacks + Deaths", "Cummulative Total")) +
      theme_dark(20)
  })
}

shinyApp(ui <- ui, server = server)