library(tidyverse)



###################################################
#
# Server for Map Tab
#
###################################################
vals <- reactiveValues(selected_tract=NULL,
                       selected_physical_category=NULL,
                       selected_health_outcome_labels=NULL,
                       selected_health_outcome_values=NULL,
                       selected_tract_total_population = NULL,
                       selected_health_outcome_scores=NULL,
                       selected_health_outcome_values_score_categories=NULL,
                       ct_overview_results=NULL
)

toListen <- reactive({
  list(input$tract_locations, input$physical_category)
})

###################################################
# Leaflet Map... https://rstudio.github.io/leaflet/
###################################################

output$map <- renderLeaflet({
  
  # Identify items to include in the map hover (str_split is used to strip out the tract information only)
  polygon_popup <- paste0(str_split(norfolk_tract_sp@data$NAME, ',', simplify = TRUE)[,1])
  
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 12,
                                   maxZoom = 12,
                                   doubleClickZoom= FALSE,
                                   scrollWheelZoom= F,
                                   dragging = FALSE,
                                   attributionControl = FALSE), 
          data = norfolk_tract_sp) %>%
    setView(-76.256, 36.897, zoom = 12) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = norfolk_tract_sp, 
                layerId = norfolk_tract_sp@data$GEOID,   # layerId is passed on map clicks
                fillColor = light_blue_color,
                fillOpacity = 0.1,
                color = dark_blue_color,
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1.0, 
                label = polygon_popup
    ) 
})



###################################################
# Observe Map Click
###################################################

#observeEvent(input$map_shape_click, {
observeEvent(input$map_shape_click, {
  
  event <- input$map_shape_click
  
  
  if(is.null(event))
    return()
  
  # Event id is the GEOID (aka fips code).  The id points to the layerId in the addPolygons in the map.
  event_id <- paste0(event$id)
  
  # Set tract name using fips code
  tract_name <- paste0(norfolk_tract_sp@data$NAME[ which(norfolk_tract_sp@data$GEOID == event_id)])
  
  # Output the tract name to ui
  output$map.tract <- renderText({ tract_name })
  
  # Update the drop down to equal the clicked map
  updateSelectInput(session, "tract_locations", selected = tract_name)
  
  
})



###################################################
# Observe Drop Down
###################################################

observeEvent(toListen(), {
  
  
  vals$selected_tract <- input$tract_locations
  vals$selected_physical_category <- input$physical_category
  
  if (vals$selected_tract %>% is.null() == FALSE)
  {
    
    # Output the tract name to ui 
    output$map.tract <- renderText({ vals$selected_tract })
    
    # Subset the dataframe to just the selected tract
    selected <- norfolk_tract_sp[norfolk_tract_sp@data$NAME == vals$selected_tract,]
    
    # Identify items to include in the proxy map hover
    polygon_popup <- paste0(str_split(selected@data$NAME, ',', simplify = TRUE)[,1])
    
    # Proxy the leaflet map
    proxy <- leafletProxy("map")
    
    
    # Add the polygons from the dataframe subset
    proxy %>% addPolygons(data = selected,
                          label = polygon_popup,
                          fillColor = dark_blue_color,
                          fillOpacity = 1,
                          color = dark_gray_color,
                          weight = 3,
                          stroke = T,
                          #layerId = selected@data$Name)
                          layerId = "Selected")
    
    
    cur_data = norva_places_data %>% 
      filter(census_tract == selected@data$GEOID[1] & physical_category == vals$selected_physical_category) %>% 
      select(physical_category, health_outcome_label, health_outcome_value, TotalPopulation) %>% unique()

    vals$selected_tract_total_population = cur_data$TotalPopulation[1] %>% format(big.mark = ",")

    
    cur_data = cur_data %>% select(-TotalPopulation)
    
    # this is a list
    vals$selected_health_outcome_values = cur_data$health_outcome_value
    vals$selected_health_outcome_labels = cur_data$health_outcome_label
    
    # TODO : PREP THESE
    vals$selected_health_outcome_scores = get_score(vals$selected_health_outcome_values, vals$selected_health_outcome_labels)
    
    #vals$selected_health_outcome_values_score_categories = get_score_category(vals$selected_health_outcome_values)
    
    vals$ct_overview_results = get_summary_tbl(vals$selected_physical_category, vals$selected_health_outcome_labels, vals$selected_health_outcome_values, vals$selected_health_outcome_scores)
  }
  
})



###################################################
# Propogate the dropdown with tract names
###################################################

output$tract_location <- renderUI({
  
  # Get unique names from data to propogate the select input
  map.tract.names <- as.vector( unique(norfolk_tract_sp@data$NAME) )
  selectInput("tract_locations",
              label = "Choose Census Tract",  
              choices=map.tract.names, 
              multiple=F)
})

output$ct_model_overview_health_data <- render_gt(
  expr = gtify_ct_overview_results(vals$ct_overview_results, vals$selected_physical_category, 
                                   vals$selected_tract, 
                                   vals$selected_tract_total_population)
)

output$domain_corrplot <- renderPlot(
  expr = get_plot_for_domain(vals$selected_physical_category, 
                             vals$selected_health_outcome_labels, 
                             vals$selected_health_outcome_values, vals$selected_health_outcome_scores)
)

output$domain_text_recs <- renderUI(
  expr = get_text_for_domain(vals$selected_physical_category, 
                             vals$selected_health_outcome_labels, 
                             vals$selected_health_outcome_values, vals$selected_health_outcome_scores)
)







