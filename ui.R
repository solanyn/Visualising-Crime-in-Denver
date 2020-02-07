library(shiny)
library(leaflet)

list <- list("All Other Crimes" = "all-other-crimes",           
     "Public Disorder" = "public-disorder",             
     "Drug & Alcohol" = "drug-alcohol",             
     "Other Crimes Against Persons" = "other-crimes-against-persons",
     "White Collar Crime" = "white-collar-crime",
     "Murder" = "murder",                      
     "Robbery" = "robbery",                     
     "Aggravated Assault" = "aggravated-assault",         
     "Arson" = "arson",                      
     "Burglary" = "burglary",                   
     "Larceny" = "larceny",                    
     "Theft from Motor Vehicle" = "theft-from-motor-vehicle",    
     "Auto Theft" = "auto-theft" )

# Define UI for application that draws a histogram
shinyUI(
    bootstrapPage(
        title = "Crime in Denver",
        
        #Layout
        div(class = "outer",
            tags$head(includeCSS("styles.css")),
            
            leafletOutput("map", width = "100%", height = "100%"),
            
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
                          h2("Crime in Denver"),
                          tags$a(href="https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime", "Source: Denver Open Data Catalog: Crime (Retrieved 2019)"),
                          sliderInput("year", h3("Year"), min = 2014, sep = "", max = 2019, value = 2014, 
                                      animate = animationOptions(interval = 1500, loop = FALSE)),
                          selectInput("cat", label = h3("Categories"),
                                      choices = list, selected = "robbery"),
                          plotOutput("bar", height = 300)
            )
        )
        
   )
)
