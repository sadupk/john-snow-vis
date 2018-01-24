library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)

#Lots of code snippets here are inspired by Andy Johnson's demos in CS424

# Read the data in tsv format
cholera_deaths = read.delim("18p1data/choleraDeaths.tsv", skip = 1, col.names = c("Date", "Attack", "Death"), header = FALSE)
cholera_age_sex = read.delim("18p1data/naplesCholeraAgeSexData.tsv", skip = 5, col.names = c("age", "male", "female"), header = FALSE)