
library(shiny)
library(rsconnect)
library(leaflet)
library(plotly)
library(data.table)
library(rgdal)
library(dplyr)

shinyUI(fluidPage(
    titlePanel("Hey Canada!"),
    sidebarLayout(
        sidebarPanel(
            h3("Quarters"),
            sliderInput("slider_quarter","Slidy Boi!",1,269,1),
            h5("The Population tab is the only tab that has
               interactivity with the slider")),
        mainPanel(
            tabsetPanel(type = "tabs", 
            tabPanel("Population", br(),
            h3("Population eh!"),
            h4("Population of some Canadian provinces by quarters
               as per Statistics Canada estimates"),
            h3("\n"),
            h4("Quarter:"),
            textOutput("QuarterText"), 
            
            leafletOutput("leaf1")),
            tabPanel("Summary", br(),h3("Summary of the regression"),h3("\n"),
            h4("Regression model: GNI = Consumption + Government
               Expenditure + Investment + Export + Import"),h3("\n"),
            verbatimTextOutput("summ")), 
            tabPanel("Plot", br(),
            h4("Relation between GNI, Consumption,
            Government Expenditure, and Investment\n\n"),h3("\n"),
            plotlyOutput("plot1")
            ))
    ))))