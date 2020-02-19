populationData <- read_csv("data/kenya-census-2019.csv")
county_data_melt<-populationData %>% melt(id.vars="County")
counties_shapefile<-readOGR("shapefiles/county.shp")



function(input, output, session) {

  output$total_pop<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Population)), 1), nsmall=1, big.mark=","),
      
      "TOTAL POPULATION",
      icon = icon("users"),
    )
  })
  
  output$male_pop<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Female_Population)), 1), nsmall=1, big.mark=","),
      "FEMALE POPULATION",
      icon = icon("female"),
    )
  })
  # format(round(as.numeric(1000.64), 1), nsmall=1, big.mark=",") 
  
  output$female_pop<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Male_Population)), 1), nsmall=1, big.mark=","),
      "MALE POPULATION", 
      icon = icon("male"),
    )
  })
  
  output$house_hold<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Number_of_Households)), 1), nsmall=1, big.mark=","),
      "NUMBER OF HOUSE HOLDS", 
      icon = icon("home"),
    )
  })
  
  output$house_hold_ize<-renderValueBox({
    valueBox(
      format(round(as.numeric( sum(populationData$Average_Household_size)/47), 1), nsmall=1, big.mark=","),
      "AVERAGE HOUSE HOLD SIZE",
      icon = icon("store"),
    )
  })
  
  output$pop_dn<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Population_Density)/47), 1), nsmall=1, big.mark=","),
     
      "POPULATION DENSITY PER KmSq", icon = icon("users"),
    )
  })
  
  output$pop_intsx<-renderValueBox({
    valueBox(
      format(round(as.numeric(sum(populationData$Intersex_population)), 1), nsmall=1, big.mark=","),
      
      "INTERSEX POPULATION ", icon = icon("user"),
    )
  })
  
  output$top5.bar <- renderPlot({
    
    plotTop5(input$choose.population)
    
  })
  

  output$down5.bar <- renderPlot({
    
    plotDownCounties(input$choose.population)
    
  })
  
  
  output$choose.population <- renderLeaflet({
    plotLeaf(input$choose.population)
    
  })

  
  
  output$data.table<-renderDataTable({
    plotDataTable()
    
  })
  
 }
