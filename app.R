library(rgdal)
library(leaflet)
library(shiny)

# Load shapefiles
blocos <- readOGR(dsn= getwd() , layer="blocos")
blocos <- spTransform(blocos, CRS("+init=epsg:4326"))
hortos <- readOGR(dsn= getwd() , layer="hortos")
hortos <- spTransform(hortos, CRS("+init=epsg:4326"))

# Generate random values for the column 'habilita'
blocos_nomes <- unique(blocos$BLOCO_L)
habilita <- data.frame(nome=blocos_nomes,habilita=sample(c(TRUE,FALSE), length(blocos_nomes), replace = TRUE))
blocos$habilita <- habilita$habilita[match(blocos$BLOCO_L,habilita$nome)]

ui <- fluidPage(
    leafletOutput("map")
)

server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        
        # Identify the group of the clicked feature
        label <- input$map_shape_click$id
        label_idx <- blocos@data$UNIQUEID %in% label
        blc <- blocos@data$BLOCO_L[label_idx]
        blc_idx <- which(blocos@data$BLOCO_L%in%blc)
        # Update the value for the column 'habilita'
        if (any(blc_idx)) {
            blocos$habilita[blc_idx] <- !blocos$habilita[blc_idx]
            blocos <<- blocos
        }
        
        # Labels
        labels <- paste("Bloco: ", blocos@data$BLOCO_L,"<br/>", "Habilitado: ", blocos$habilita, sep="")
        
        # Colors
        pal <- colorFactor(palette = c('red','green'), levels = c(FALSE,TRUE))
        
        # Show map
        leaflet(blocos) %>% 
            addProviderTiles(providers$CartoDB.Positron)  %>% 
            addPolygons(data=hortos,weight=2,fillOpacity = 0, color = 'black')%>%
            addPolygons(
                fillColor = ~pal(habilita),
                layerId = ~UNIQUEID,
                label = lapply(labels, htmltools::HTML),
                weight=.3,
                opacity = 1,
                color='black',
                highlightOptions = highlightOptions(weight = 5, fillOpacity = 1, bringToFront = TRUE)
            ) %>%
            addLegend(
                pal = pal,
                values = ~na.omit(unique(habilita)),
                title = "Bloco", 
                position = "bottomright" 
            )
    })
    
}

shinyApp(ui, server)