#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$topCounties.bar <- renderPlot({
        
        plotTopCounties(input$choose.population)
        
    })
    
    
    output$dwnCounties.bar <- renderPlot({
        
        plotDownCounties(input$choose.population)
        
    })
    
    
    output$choose.population <- renderLeaflet({
        plotLeaf(input$choose.population)
        
    })
    
 
    # observeEvent(input$choose.population,{
    #     leafletProxy("leaf") %>% clearControls()
    # })
    
    output$data.table<-renderDataTable({
        
       plotDataTable()
        
    })
    
    
    
    
})
