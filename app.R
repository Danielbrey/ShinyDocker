library(shiny)
library(tidyverse)
library(rvest)
library(shinydashboard)
library(DT)
library(igraph)

ui <- fluidPage(


)


server <- function(input, output) {
    output$force <- renderForceNetwork({
        if(input$chooseGroup == "full"){
            filteredLinks <- MisLinks
            filteredNodes <- MisNodes
        }
        if(input$chooseGroup != "full"){
            filteredNodes <- MisNodes %>% 
                filter(group == input$chooseGroup)
            filteredLinks <- MisLinks %>% 
                filter((sourceGroup == input$chooseGroup) & (targetGroup == input$chooseGroup))
        }
        if(input$dataAnalysis == "Closeness"){
            forceNetwork(Links = filteredLinks, Nodes = filteredNodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name",
                         Group = input$grouping, opacity = 0.8,
                         Nodesize = "closeSquared")
            
        } else if(input$dataAnalysis == "Betweenness"){
            forceNetwork(Links = filteredLinks, Nodes = filteredNodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name",
                         Group = input$grouping, opacity = 0.8,
                         Nodesize = "between")
        }
        
    })
    
    output$nodes <- renderTable(MisNodes)
    output$links <- renderTable(MisLinks)
    output$nodelist <- renderPrint(typeof(MisNodes))
    
}


shinyApp(ui = ui, server = server)
