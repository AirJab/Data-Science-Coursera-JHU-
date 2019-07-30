
library(shiny)
library(plotly)
library(dplyr)
library(rsconnect)
library(leaflet)
library(data.table)
library(rgdal)

shinyServer(function(input, output) {
#Part-1 : Population

    prov.pop <- fread("./can_pop.csv",stringsAsFactors = FALSE)
    
    prov.pop[] <- lapply(prov.pop, function(x) gsub(",","",x))
    
    prov.pop[] <- lapply(prov.pop, function(x) as.character(x))
    
    prov.pop[,2:272] <- lapply(prov.pop[,2:272],
                               function(x) as.numeric(x))
    region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes",
              encoding='UTF-8')

    
    #Part - 2 : Regression
    indicator <- data.table::fread("./can_indicators.csv")
    
    indicator.1 <- data.frame(t(indicator))
    
    colnames(indicator.1) <- as.character(unlist(indicator.1[1,]))
    indicator.1 = indicator.1[-1, ]
    
    colnames(indicator.1) <- c("Year","GNI","EXP" ,"IMP","CONS","GEXP","INV")
    
    indicator.1[] <- lapply(indicator.1, function(x) as.numeric(as.character(x)))
    
    indicator.1[,-1]<- lapply(indicator.1[,-1],function(x) log(x))
    
    
    
    #REactiveS
    
    data.pop <- reactive({prov.pop})
    
    data.region <- reactive({region})
    
    
    data.names <- reactive({names.1 <- colnames(data.pop())})
    
    data.model <- reactive({lm(GNI~.-Year, data=indicator.1)})
    
    
    
    output$QuarterText <- renderText(
        data.names()[input$slider_quarter+3])

    
    output$leaf1 <- renderLeaflet({
        
        data.pop() %>% leaflet() %>% 
            addTiles() %>% 
            setView(-100, 62,  zoom = 3) %>% 
            addPolygons(data = subset(data.region(),
            name %in% c("Quebec","British Columbia"
            , "Alberta", "Saskatchewan", "Manitoba", "Ontario", 
            "Quebec", "New Brunswick", "Prince Edward Island", "Nova Scotia",
            "Newfoundland and Labrador", "Yukon", "Northwest Territories", 
            "Nunavut")),
            fillColor = topo.colors(15, alpha = NULL),weight = 1) %>%
            addCircles(popup = paste0(data.pop()[[1]]),#popup = paste(data.pop()$Province)
                    weight = 4,
            radius = (data.pop()[[input$slider_quarter+3]])*.02
            , color = "red")
    })
    

    output$summ <- renderPrint({summary(data.model())})
    
    output$plot1 <- renderPlotly({
    p <- plot_ly(data = indicator.1, x = ~CONS, y = ~GNI,
    size = ~INV,color=~GEXP) %>%
    layout(title="GNI, Consumption, Govt. Expenditure, and Investment for Canada",
    xaxis= list(title="Consumption"),yaxis=list(title="Gross National Income"))
        
        
    p <- layout(p, annotations = 
    list(x=1.05,y=.1,text = "Data: World Bank Databank\n
    -The size of the scatter represents Investment\n
    -The colour represents Government Expenditure", 
    showarrow = F, xref='paper', yref='paper', 
    xanchor='right', yanchor='auto', xshift=1, yshift=1,
    font=list(size=10, color="black")))
    p
    })
    
})