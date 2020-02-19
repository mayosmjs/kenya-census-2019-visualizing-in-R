library(leaflet)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(htmltools)






populationData <- read_csv("data/kenya-census-2019.csv")
county_data_melt<-populationData %>% melt(id.vars="County")
counties_shapefile<-readOGR("shapefiles/county.shp")


plotDataTable <- function(){
  datatable( populationData,
             class = 'cell-border stripe',
             editable = TRUE,
             options = list(scrollX = T)
  ) 
  
}




plotTop5 <- function(x ="Population",num=5){
  tpcnt <- county_data_melt %>% filter(variable == x) %>% top_n(num)
  ggplot(tpcnt,aes(x= reorder(County,value),y = value,fill= County))+
    geom_bar(stat = "identity")+
    scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) +
    xlab("County") +
    ylab(x) +
    ggtitle(paste0(num," Counties with the most ",x)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme_minimal()
  
}


plotDownCounties <- function(x = "Population",num = 5){
  dwncnt <- county_data_melt %>% filter(variable == x) %>% top_n(-num)
  ggplot(dwncnt,aes(x= County,y = value,fill= County))+
    geom_bar(stat = "identity")+
    xlab("County") +
    ylab(x) +
    ggtitle(paste0(num," Counties with least ",x)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme_minimal()
}


plotLeaf <- function(x = 'Population'){
  
  leaflett <- leaflet(counties_shapefile) %>% addTiles() %>%setView(lng=37.9083,lat=0.1769,zoom = 7) 
  
  if(x == 'Population'){
    leaflett %>% addPolygons(
      color = ~pal0(Popultn),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Total Population:</strong>",counties_shapefile$Popultn
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Total Population:</strong>",Popultn
      )
      
    )%>% 
      addLegend(title = "Total Population Per County",pal = pal0,
                values = counties_shapefile$Popultn,
                opacity = 1,
                position = "bottomleft")
  }
  
  else if(x == 'Land_Area(Km2)'){
    leaflett %>% addPolygons(
      color = ~pal1(L_A.K2.),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Total Area:</strong>",counties_shapefile$L_A.K2.
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Total Area:</strong>",L_A.K2.
      )
      
    )%>% 
      addLegend(title = "Total Area Per County",pal = pal1, values = counties_shapefile$L_A.K2.,
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  else if(x == 'Population_Density'){
    leaflett %>% addPolygons(
      color = ~pal2(Ppltn_D),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Population Density:</strong>",counties_shapefile$Ppltn_D
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Population Density:</strong>",Ppltn_D
      )
      
    )%>% 
      addLegend(title = "Population Density Per Sq Km",pal = pal2, values = counties_shapefile$Ppltn_D, opacity = 1,position = "bottomleft")
    
    
  }
  
  
  else if(x == 'Male_Population'){
    leaflett %>% addPolygons(
      color = ~pal3(Ml_Pplt),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Male Population:</strong>",counties_shapefile$Ml_Pplt
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Population Density:</strong>",Ml_Pplt
      )
      
    )%>% 
      addLegend(title = "Male Population Per County",pal = pal3, values = counties_shapefile$Ml_Pplt, 
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  
  else if(x == 'Female_Population'){
    leaflett %>% addPolygons(
      color = ~pal4(Fml_Ppl),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Female Population:</strong>",counties_shapefile$Fml_Ppl
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Female Population:</strong>",Fml_Ppl
      )
      
    )%>% 
      addLegend(title = "Female Population Per County",pal = pal4, values = counties_shapefile$Fml_Ppl, 
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  
  
  else if(x == 'Intersex_population'){
    leaflett %>% addPolygons(
      color = ~pal5(Intrsx_),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Intersex Population:</strong>",counties_shapefile$Intrsx_
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Female Population:</strong>",Intrsx_
      )
      
    )%>% 
      addLegend(title = "Intersex Population Per County",pal = pal5, values = counties_shapefile$Intrsx_,
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  else if(x == 'Number_of_Households'){
    leaflett %>% addPolygons(
      color = ~pal6(Nmbr__H),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Number of households:</strong>",counties_shapefile$Nmbr__H
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Number of households:</strong>",Nmbr__H
      )
      
    )%>% 
      addLegend(title = "Number of households Per County",pal = pal6, values = counties_shapefile$Nmbr__H,
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  else if(x == 'Average_Household_size'){
    leaflett %>% addPolygons(
      color = ~pal7(Avrg_H_),
      smoothFactor = 0.6,
      weight = 1, 
      opacity = 1.0,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "purple",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = paste(
        "<strong>County:</strong>",counties_shapefile$ADM1_EN,
        "<br>",
        "<strong>Average household size:</strong>",counties_shapefile$Avrg_H_
        
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 6px"), 
                                   textsize = "12px", direction = "auto"),
      
      popup = ~paste(
        "<strong>County:</strong>",ADM1_EN,
        "<br>",
        "<strong>Female Population:</strong>",Avrg_H_
      )
      
    )%>% 
      addLegend(title = "Average household size Per County",pal = pal7, values = counties_shapefile$Avrg_H_, 
                opacity = 1,
                position = "bottomleft")
    
    
  }
  
  
  
  
}#Endof leaflet function


#custom palette colors

#Population 
pal0<-colorBin("Set2", counties_shapefile$Popultn)
#Area density
pal1 <- colorBin("YlOrBr",counties_shapefile$L_A.K2.)
#Population density
pal2<-colorBin("YlOrBr", counties_shapefile$Ppltn_D)
#Population of male 
pal3<-colorBin("Blues", counties_shapefile$Ml_Pplt)
#Population of female 
pal4<-colorBin("Set3", counties_shapefile$Fml_Ppl)
#Population of intersex
pal5<-colorBin("YlOrBr", counties_shapefile$Intrsx_)
#Number of HOuseholds
pal6<-colorBin("YlOrBr", counties_shapefile$Nmbr__H)
#Household size
pal7<-colorBin("BuPu", counties_shapefile$Avrg_H_)







  navbarPage("KENYA CENSUS 2019", id="nav",theme = shinytheme("flatly"),
             tabPanel("Interactive map",icon = icon("map"),
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          # leafletOutput("map", width="100%", height="100%"),
                          leafletOutput("choose.population", width = "100%", height = "100%"),
                          
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-primary", fixed = TRUE,
                                        draggable = TRUE, top = 80, left = "auto", right = 40, bottom = "auto",
                                        width = 600, height = "auto",
                                        
                                        h2("KENYA CENSUS 2019"),
                                        h4("Author: Calvin Boore"),
                                        
                                        selectInput("choose.population", "Choose a population parameter to explore",
                                                    choices = unique(county_data_melt$variable),
                                                    selected = "Population"),
                                
                                        
                                        
                                        plotOutput("top5.bar", height = 200),
                                        plotOutput("down5.bar", height = 250),
                                        
                                        hr(),
                                        
                                        h3("Snapshot"),
                                        
                                        fluidRow(
                                          
                                          valueBoxOutput(
                                            "total_pop"
                                            
                                          ),
                                          
                                          valueBoxOutput(
                                            "female_pop"
                                            
                                          ),
                                          
                                          valueBoxOutput(
                                            "male_pop"
                                            
                                          ),
                                          
                                          valueBoxOutput(
                                            "pop_intsx"
                                            
                                          ),
                                          
                                         
                                          
                                          valueBoxOutput(
                                            "house_hold"
                                            
                                          ),
                                          valueBoxOutput(
                                            "house_hold_ize"
                                            
                                          ),
                                          
                                          valueBoxOutput(
                                            "pop_dn"
                                            
                                          )
                                          
                                        ),
                          ),
                          
                          
                      )
             ),
             
             tabPanel("Data explorer",icon = icon("book"),
                    
                      h2("2019 KENYA POPULATION CENSUS TABLE"),
                      hr(),
                      dataTableOutput("data.table")
             )
  )

