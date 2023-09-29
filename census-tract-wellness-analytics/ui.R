



###################################################
#
# The ui.R is designed to use separate files for each 
# tab in the UI.  The ui tab files are located in the
# ui folder.  Each file is accessed via "source" (see
# tab panel 2 "Map").  
#
###################################################




shinyUI(navbarPage(title = "Exploring Data in Swarbrick's Six Domains of Physical Health at the Census Tract Level in Norfolk, VA",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   ####################################
                   # tab panel - Census Tract Overview
                   ####################################
                   tabPanel("Data Exploration", source(here::here("ui", "ui_ct_overview.R"),  local = TRUE)$value),
                   
                   
                   ####################################
                   # tab panel - About
                   ####################################
                   tabPanel("About", source(here::here("ui", "ui_about_short.R"), local=TRUE)$value)
))




