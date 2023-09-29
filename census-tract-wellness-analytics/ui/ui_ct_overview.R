



###################################################
#
# UI for Map Tab
#
###################################################

tagList(
  div(class = "container-fluid",
      #h1("Neighborhood Browser"),
      # fluidRow(
      #   column(width = 2,
      #          uiOutput('map_location')
      #   )
      # ),
      fluidRow(
        #  Turn the leaflet background to white
        tags$head(
          tags$style(HTML(".leaflet-container { background: #ffffff; }"))
        ),
        column(width = 3,
               uiOutput('tract_location'),
               selectInput("physical_category", "Select Phyiscal Health Domain To Explore", choices = physical_category_choices, 
                           selected = default_physical_category_choice)
        )
      ),
      fluidRow(
        column(5,
               leafletOutput("map", height = 700)
        ),
        # column(width = 2, #offset = 1,
        #        gt_output(outputId = "census_pop_data")
        # ),
        column(width = 5, #offset = 1,
               tabsetPanel(
                 tabPanel("Health Data", gt_output(outputId = "ct_model_overview_health_data")),
                 tabPanel("Improving Census Tract Health Outcomes", plotOutput(outputId = "domain_corrplot"), htmlOutput(outputId = "domain_text_recs"))
               )
        )
      )
  )
)
