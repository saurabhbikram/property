server <- function(input, output, session) {
  
  all_data <- reactive({
    withProgress({
      raw_data <- bind_rows(load_data("ec"),load_data("sw11"))
    }, message="loading data...")
  })
  
  value_model <- reactive({
    withProgress(adjusted_values_model(raw_data), message="getting price model")
  })
  
  price_level <- reactive({
    plevel <- raw_data %>% group_by(latitude, longitude) %>% summarise(value = mean(price_num/area,na.rm=T)) %>% na.omit()
    plevel
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    POSTCODE <- input$postcode
    BEDS <- input$beds
    DIST <- input$radius
    DATE <- as.Date(input$transdate)
    FS <- (input$for_sale == "for sale")
    house_data <- apply_property_filter(prices_data2 = all_data(),
                                        postcode =  POSTCODE,beds =  BEDS,dist =  DIST,
                                        transdate =  DATE, for_sale =  FS, map_bounds = input$map_bounds)
    if(nrow(house_data)) make_pretty_df(house_data) else NULL
  })
  
  output$map <- renderLeaflet({
    
    raw_data <- all_data() 
    
    if(input$layer_type=="adjusted") {
      shdf <- get_adjusted_model(raw_data %>% filter(!is.na(longitude) & !is.na(latitude)), value_model())
    } else {
      shdf <- make_df_for_spatial(raw_data)
    }
    
    map <- leaflet(data=raw_data) %>% 
      addTiles() %>% 
      apply_spatial_values(get_spatial_df(shdf, breaks=input$layer_breaks)) %>% 
      setView(mean(raw_data$longitude,na.rm=T),
              mean(raw_data$latitude,na.rm=T),
              zoom=15)
    return(map)
  })
  
  observe({
    req(filteredData())
    input$layer_type
    input$layer_breaks
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>% 
      addMarkers(lng = ~longitude,lat = ~latitude, 
                       clusterOptions = markerClusterOptions(),
                       label=~price_pretty, popup=~msg, 
                        #color=~bg_color,
                       labelOptions = labelOptions(noHide = T, textsize = "12px",direction = "bottom"))
      
  })
  
  
}
