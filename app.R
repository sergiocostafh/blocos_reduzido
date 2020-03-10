#APP BACKUP SEM REATIVIDADE

library(rgdal)
library(leaflet)
library(shiny)

##Read the data
blocos <- readOGR(dsn= getwd() , layer="blocos")
blocos <- spTransform(blocos, CRS("+init=epsg:4326"))
hortos <- readOGR(dsn= getwd() , layer="hortos")
hortos <- spTransform(hortos, CRS("+init=epsg:4326"))
#Create the objective vector with random values
blocos_nomes <- unique(blocos$BLOCO_L)
habilita <- data.frame(nome=blocos_nomes,habilita=sample(c(TRUE,FALSE), length(blocos_nomes), replace = TRUE))
blocos$habilita <- habilita$habilita[match(blocos$BLOCO_L,habilita$nome)]

ui <- fluidPage(
    leafletOutput("map")
)

server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        
        # identificar o bloco clicado
        label <- input$map_shape_click$id
        label_idx <- blocos@data$UNIQUEID %in% label
        blc <- blocos@data$BLOCO_L[label_idx]
        blc_idx <- which(blocos@data$BLOCO_L%in%blc)
        # atualizar coluna
        if (any(blc_idx)) {
            blocos$habilita[blc_idx] <- !blocos$habilita[blc_idx]
            blocos <<- blocos
        }
        
        #Labels
        labels <- paste("Bloco: ", blocos@data$BLOCO_L,"<br/>", "Habilitado: ", blocos$habilita, sep="")
        
        #Cores
        pal <- colorFactor(palette = c('red','green'), levels = unique(blocos$habilita))
        pal2 <- colorFactor(palette = c('black','red'), levels = unique(blocos$PIC2020))
        
        #Show map
        leaflet(blocos) %>% 
            addProviderTiles(providers$CartoDB.Positron)  %>% 
            addPolygons(data=hortos,weight=2,fillOpacity = 0, color = 'black')%>%
            addPolygons(
                fillColor = ~pal(habilita),
                layerId = ~UNIQUEID,
                label = lapply(labels, htmltools::HTML),
                weight=.3,
                opacity = 1,
                color=~pal2(PIC2020),
                highlightOptions = highlightOptions(weight = 5, fillOpacity = 1, bringToFront = TRUE)
            ) %>%
            addLegend(
                pal = pal,
                values = ~na.omit(unique(habilita)),
                title = "Bloco Lidar", 
                position = "bottomright" 
            )
    })
    
}

shinyApp(ui, server)