# Kus Server ----
server <- function(input, output, session) {

  showModal(modalDialog(
    title = "Snake IPTDS Use Agreement",
    tags$div("The Snake River Basin IPTDS web application is currently in development. Please be patient while the organization changes and content is added. The application uses live data obtained from the Nez Perce Tribe Department of Fisheries Resources Management's Centralized Data Managment System (CDMS). More information regarding estimation methods and metadata are available in technical reports for reference at ", tags$a(href="http://kus.nptfisheries.org/kus-data/",
    "http://kus.nptfisheries.org/kus-data/"
  ))
))

output$spp_text <- renderText({
  paste('Snake Basin', input$basin_spp, 'IPTDS Estimates') 
        })

observe({    
  spp <- input$basin_spp
iptds_server("stadem", spp)
})

#iptds_server("chinook", 'Chinook salmon')

# Get map data ----
map_data <- eventReactive(input$basin_spp, {
  
  mapFun(input$basin_spp)
  
})

# Create basemap ----
# Basic NPT map area and Snake Basin

output$map <- renderLeaflet({
  leaflet() %>%
    #addTiles() %>%
    setView(lng = -115.5660,
            lat = 45.4000,#44.9218,
            zoom = 7) %>%
    addProviderTiles(providers$Esri.WorldTopoMap)%>%
    addPolylines(data = pnw_rivers, color = 'blue', weight = 1) %>%
    addPolylines(data = stream_layer, color = 'blue', weight = 1)
})
 
# proxy map----
observeEvent(input$basin_spp, {
  
  pop_copop <- colorFactor(palette = 'viridis', domain = map_data()$pop_layer$MPG)

  pal <- colorFactor(
    palette = topo.colors(9)[1:6],
    domain = map_data()$site_est_layer$SiteTypeName
  )
  
leafletProxy('map', session) %>%
  clearGroup('Populations') %>%
  clearGroup('Sites') %>%
  removeControl("legend") %>% 
  removeLayersControl() %>%
  
  addPolygons(data = map_data()$pop_layer,
              label = ~TRT_POPID,
              layerId = ~TRT_POPID,
              group = 'Populations',
              popup = popupGraph(map_data()$p_all),
              labelOptions = labelOptions(noHide = F, textsize = 12),
              fillColor = ~pop_copop(MPG), weight = 1, fillOpacity = .5) %>%
  addPolygons(data = map_data()$full_pop, group = 'Populations', fill = NA, stroke = TRUE,
              color = 'black', weight = 2, opacity = 1, fillOpacity = 0) %>%
  addCircleMarkers(data = map_data()$site_est_layer, radius = 5,
                   group = 'Sites',
                   popup = map_data()$site_tab,
                   color = ~pal(SiteTypeName),
                   label = ~SiteID,
                   layerId = ~SiteID,
                   labelOptions = labelOptions(noHide = F),
                   popupOptions = popupOptions(noHide = F, minWidth = 400, maxWidth = 400)) %>%
  addLegend(data = map_data()$site_est_layer, position = "bottomleft", pal = pal, values = ~SiteTypeName,
            title = "Observation Type",
            opacity = 1,
            layerId = 'legend') %>%
  addLegend(data = map_data()$pop_layer, position = "bottomleft", pal = pop_copop, values = ~MPG,
            title = "Major Population Group",
            opacity = .5,
            layerId = 'legend2') %>%
  addLayersControl(
    overlayGroups = c("Populations", "Sites"),
    options = layersControlOptions(collapsed = FALSE)
  )

})

output$raw_data <- DT::renderDataTable({
    #export_dat()
    map_data()$site_est_spp},
  options = list(pageLength = 100)
  )

  # function for downloading data
  output$data_export <- downloadHandler(  #output name needs to match ui object id name

    filename = function() {
      #paste0(rawdat(),"_", Sys.Date(), "_.csv")
      paste0("iptds_estimates","_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(site_est_spp, filename, row.names = FALSE)
    }
  )
  
} # close Server
