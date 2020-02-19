#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(leaflet)
library(readr)
library(htmltools)
library(rgdal)
library(viridis)
library(RColorBrewer)
library(wesanderson)
# colors  https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/







populationData <- read_csv("data/kenya-census-2019.csv")
county_data_melt<-populationData %>% melt(id.vars="County")
counties_shapefile<-readOGR("shapefiles/county.shp")




###############################FUNCTIONS###########################################
################################FUNCTIONS#########################################


plotDataTable <- function(){
        datatable( populationData,
                   class = 'cell-border stripe',
                   editable = TRUE,
                   options = list(scrollX = T)
        ) 
    
}


plotTopCounties <- function( x = "Population",num = 5){
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
    
leaflett <- leaflet(counties_shapefile) %>% addTiles() %>%setView(lng=37.9083,lat=0.1769,zoom = 6) 

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
        addLegend(title = "Total Population Per County",pal = pal0, values = counties_shapefile$Popultn, opacity = 1)
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
            addLegend(title = "Total Area Per County",pal = pal1, values = counties_shapefile$L_A.K2., opacity = 1)
    
        
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
        addLegend(title = "Population Density Per Sq Km",pal = pal2, values = counties_shapefile$Ppltn_D, opacity = 1)
    
    
}


else if(x == 'Male_Population'){
    leaflett %>% addPolygons(
        color = ~pal4(Ml_Pplt),
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
        addLegend(title = "Male Population Per County",pal = pal3, values = counties_shapefile$Ml_Pplt, opacity = 1)
    
    
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
        addLegend(title = "Female Population Per County",pal = pal4, values = counties_shapefile$Fml_Ppl, opacity = 1)
    
    
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
        addLegend(title = "Intersex Population Per County",pal = pal5, values = counties_shapefile$Intrsx_, opacity = 1)
    
    
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
        addLegend(title = "Number of households Per County",pal = pal6, values = counties_shapefile$Nmbr__H, opacity = 1)
    
    
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
        addLegend(title = "Average household size Per County",pal = pal7, values = counties_shapefile$Avrg_H_, opacity = 1)
    
    
}







}#Endof leaflet function



#custom palette colors

#Population 
pal0<-colorBin("YlOrBr", counties_shapefile$Popultn)
#Area density
pal1 <- colorBin("YlOrBr",counties_shapefile$L_A.K2.)
#Population density
pal2<-colorBin("YlOrBr", counties_shapefile$Ppltn_D)
#Population of male 
pal3<-colorBin("YlOrBr", counties_shapefile$Ml_Pplt)
#Population of female 
pal4<-colorBin("YlOrBr", counties_shapefile$Fml_Ppl)
#Population of intersex
pal5<-colorBin("YlOrBr", counties_shapefile$Intrsx_)
#Number of HOuseholds
pal6<-colorBin("YlOrBr", counties_shapefile$Nmbr__H)
#Household size
pal7<-colorBin("YlOrBr", counties_shapefile$Avrg_H_)





# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("sandstone"),

 
    # NAVBAR
    navbarPage("KENYA CENSUS 2019 ",
               
               tabPanel("Interactive map",icon = icon("map"),
                        
                        # SIDEBAR LAYOUT
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "choose.population",
                                            label = "Choose a population parameter to explore",
                                            choices = unique(county_data_melt$variable),
                                            selected = "Population"),
                                hr(),
                                
                                plotOutput("topCounties.bar", height = 300),
                                
                                hr(),
                                
                                plotOutput("dwnCounties.bar", height = 300),
                                
                            ),
                            
                            
                            
                            #MAIN PANEL (mainpanel needs to be called inside the sideBarLayout)
                            mainPanel(
                                h1("2019 KENYA POPULATION CENSUS"),
                                h3("An Interactive Dashboard"),
                                h3("Author: Calvin Boore"),
                                
                                br(),
                                
                                leafletOutput("choose.population", width = "100%", height = "850px"),
                                
                            )
                            #ENDOFMAIN
                            
                            
                            
                        ),
                        #END SIDEBAR LAYOUT
                        ),#END OF TABPANEL
               
               
               
               
               tabPanel("Data",icon = icon("book"),
                       
                       column(12,
                              h1("2019 KENYA POPULATION CENSUS DATATABLE"),
                              h3("Author: Calvin Boore"),
                              br(),
                              hr(),
                              
                              dataTableOutput("data.table")
                       )
                    ),#END TABPANEL 2
               
               
               tabPanel("About",icon = icon("pencil"),
                        
                       column(6,
                              h1("2019 Kenya Population and Housing Census Results"),
                              h4("Extracted From https://www.knbs.or.ke/?p=5621"),
                              p("The total enumerated population was 47,564,296 Of which 23,548,056 were Males, 24,014,716 were Females and 1,524 were Intersex Females accounted for 50.5% of the total population
                        
                        The population has grown to 47.6 Million in 2019 from 37.7 Million in 2009 The intercensal growth rate has declined to 2.2% in 2019, from 2.9% in 2009
                        
                        Average Household Size has declined to 3.9 in 2019 from 4.2 in 2009
                        
                        The 2019 Kenya Population and Housing Census was the 8th to be conducted in Kenya and the first paperless, where Mobile technology was used during mapping and enumeration, in adherence to the UN recommendations for the 2020 round of censuses on adoption of use of technology.
                        
                        The 2019 Census was conducted under the provisions of the Constitution of Kenya, 2010 (Fourth Schedule Part 1 Item 11), the Statistics (Amendment) Act, 2019 and the Statistics (Census of Population) Order, 2018 – Legal Notice No. 205. The theme of the census was “Counting Our People for Sustainable Development and Devolution of Services”. This was in response to the demand for statistical information for implementation of Kenya’s development agenda such as the Big Four and Vision 2030 and other global initiatives including the Sustainable Development Goals (SDGs).
                        
                        The preparations for the 2019 census commenced in 2016 with Cartographic Mapping. Other preparatory activities undertaken included development of tools, pilot census, recruitment and training, and publicity and advocacy.
                        
                        The enumeration was successfully conducted from 24th/25th to 31st August 2019 and a mop-up exercise carried out on 1st and 2nd of September 2019, to cover those not enumerated during the seven days. To ensure data quality, field supervision followed a three tier structure (Coordinators, ICT and Content Supervisors) to support real-time response to emerging issues.
                        
                        The enumeration process was also monitored by independent observers drawn from the international community and national statistics offices across Africa. The collected data was encrypted and successfully transmitted to servers through a secure ICT infrastructure.
                        
                        The data that was collected has undergone validation checks as guided by the UN Fundamental Principles of Official Statistics and the UN Handbook on Editing of Census Data.
                        ")
                              
                              )
                        )#END TABPANEL 3
               
               
              
               
    ), 
    #ENDOFNAVBAR
    
    
    
   
    
   
    
    # 
    # 
    # 
    
    
    
    
    
))
