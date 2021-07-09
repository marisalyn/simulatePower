library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(bslib)
library(tidyverse)
library(reactable)
library(plotly)

source("simulatePowerFunction.R")

pal = c(
  "white" = "#FFFFFF",
  "black" = "#000000",
  "pink" = "#da7fc7", 
  "blue" = "#00c2de",
  "green" = "#94d600", 
  "yellow" = "#ffd800", 
  "purple" = "#bc84cb", 
  "orange" = "#ff8500"
)

#TODO:
# intro text
# add trtFrac option
# add better loading