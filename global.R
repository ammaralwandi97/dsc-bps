# Library

library(shiny)
library(shinydashboard)
library(DT) # datatable

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # koleksi beberapa package R
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
library(lubridate)
library(srvyr)
library(foreign)
library(shinyjs)
library(shinyWidgets)

load("ega.RData")