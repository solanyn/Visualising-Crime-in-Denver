library(sf)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(leaflet)

# Data sourced from: 
# Crime
# https://www.denvergov.org/media/gis/DataCatalog/crime/csv/offense_codes.csv
# Statistical Neighbourhoods
# https://www.denvergov.org/media/gis/DataCatalog/statistical_neighborhoods/shape/statistical_neighborhoods.zip

neighbourhoods <- read_sf("data/statistical_neighborhoods/statistical_neighborhoods.shp")
crime <- read_csv("data/crime.csv")
crime <- crime[complete.cases(crime),]
crime <- crime %>% filter(IS_CRIME == 1)
crime$OFFENSE_ID <- as.character(crime$OFFENSE_ID)
crime$REPORTED_DATE <- mdy_hms(crime$REPORTED_DATE)
neighbourhoods$NEIGHBORHOOD_ID <- str_to_lower(neighbourhoods$NBHD_NAME)
neighbourhoods$NEIGHBORHOOD_ID <- neighbourhoods$NEIGHBORHOOD_ID %>% str_replace_all(" ", "-")
neighbourhoods$NEIGHBORHOOD_ID <- neighbourhoods$NEIGHBORHOOD_ID %>% str_replace_all("---", "-")

shinyServer(function(input, output, session) {
    
    #Reactive dataframes
    crimeyear_reactive <- reactive({
        return(crime %>% filter(year(REPORTED_DATE) == input$year))
    })
    
    crimecatyear_reactive <- reactive({
        return(crime %>% filter(year(REPORTED_DATE) == input$year) %>% filter(OFFENSE_CATEGORY_ID == input$cat))
    })
    
    crimesum_reactive <- reactive({
        crimesum_r <- crimeyear_reactive() %>% 
            group_by(OFFENSE_CATEGORY_ID) %>% 
            summarise(count = n())
        crimesum_r$OFFENSE_CATEGORY_ID <- crimesum_r$OFFENSE_CATEGORY_ID %>% factor(levels = crimesum_r$OFFENSE_CATEGORY_ID[order(-crimesum_r$count)])
        selected <- numeric(length(crimesum_r$OFFENSE_CATEGORY_ID))
        selected[which(crimesum_r$OFFENSE_CATEGORY_ID == input$cat)] <- 1
        crimesum_r$selected <- selected
        
        return(crimesum_r)
    })
    
    neighbourhoods_reactive <- reactive({
        count <- crimecatyear_reactive() %>% group_by(NEIGHBORHOOD_ID) %>% 
            summarise(n = n())
        neigh <- neighbourhoods %>% left_join(count, by = "NEIGHBORHOOD_ID")
        neigh <- neigh %>% mutate(n = ifelse(is.na(n), 0, n))
        
        return(neigh)
    })
    
    labels_reactive <- reactive({
        neigh_df <- neighbourhoods_reactive()
        labels <- sprintf("<strong>%s</strong><br/> %s %s crimes in %s",
                          neigh_df$NBHD_NAME,
                          neigh_df$n,
                          str_to_title(str_replace_all(input$cat, "-", " ")), input$year) %>%
            lapply(htmltools::HTML)
        return(labels)
    })
    
    #Initial leaflet view
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng = -104.95, lat = 39.73, zoom = 12) %>%
            addProviderTiles("MapBox",
                             options = providerTileOptions(id = "mapbox.streets",
                                                           noWrap = FALSE,
                                                           accessToken = MAPBOX_ACCESS_TOKEN))
    })
    
    #Add neighbourhoods layer
    observe({
        neigh_df <- neighbourhoods_reactive()
        lab <- labels_reactive()
        
        leafletProxy("map", session) %>%
        clearGroup("neighbourhoods") %>%
        addPolygons(group = "neighbourhoods",
            data = neigh_df,
            color = "#4575b4",
            opacity = 0.1,
            highlight = highlightOptions(fillColor = "#4575b4",
                                         weight = 3,
                                         fillOpacity = 0.4
            ),
            label = lab,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "11px",
                direction = "auto"
            ))
    })
    
    #Draw barplot
    output$bar <- renderPlot({
        crimesum <- crimesum_reactive()
        ggplot(data = crimesum, aes(x = OFFENSE_CATEGORY_ID, y = count, fill = factor(selected))) + 
            theme_minimal() +
            geom_bar(stat = "identity") + 
            scale_fill_manual(values = c("#4575b4", "#d7301f")) + 
            ylim(0, 7600) +
            geom_text(aes(label=count), vjust = -0.5, size = 2.5) +
            theme(legend.position = "none", 
                  axis.text.x=element_text(angle=45,hjust=1),
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank(),
                  panel.grid.minor.x = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  axis.text.x.top = element_text(size = 9))
    })
    
    #Show crime details on click
    showCrimePopup <- function(id, lat, lng) {
        selectedCrime <- crime[crime$OFFENSE_ID == id,]
        content <- as.character(tagList(
            tags$h4("Incident ID: ", selectedCrime$INCIDENT_ID),  
            tags$strong(str_to_title(selectedCrime$OFFENSE_TYPE_ID)),
            tags$br(),
            sprintf("First Occurrence Date: %s", selectedCrime$FIRST_OCCURRENCE_DATE), tags$br(),
            sprintf("Last Occurrence Date: %s", selectedCrime$LAST_OCCURRENCE_DATE), tags$br(),
            sprintf("Incident Address: %s", selectedCrime$INCIDENT_ADDRESS)
        ))
        leafletProxy("map", session) %>% addPopups(lng, lat, content, layerId = id)
    }
    
    #Crime details on click for each point
    observe({
        #leafletProxy("map", session) %>% clearPopups()
        event <- input$map_marker_click
        if (is.null(event$id))
            return()
        isolate({
            showCrimePopup(event$id, event$lat, event$lng)
        })
    })
    
    #Change markers based on crime selected
    observe({
        crimecatyear <- crimecatyear_reactive()
        leafletProxy("map", session, data = crimecatyear) %>%
            clearMarkers() %>%
            addCircleMarkers(layerId = ~OFFENSE_ID,
                             radius = 10, color = "#d7301f", stroke = FALSE, fillOpacity = 0.5,
                             lng = crimecatyear$GEO_LON, lat = crimecatyear$GEO_LAT)
    })
    
})
