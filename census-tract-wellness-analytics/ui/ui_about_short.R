fluidPage(
  
  titlePanel("VMASC / ODU Submission to Hampton Roads Datathon 2023"),
  tags$br(),
  navlistPanel(widths = c(2, 8),
               tabPanel("Background",
                        HTML('<center><img src="Wellness-Dimensions-And-Physical-Health-Domains.png", width="1000", height="600"></center>'),
                        tags$br(),
                        tags$br(),
                        h4("Background"),
                        p(paste("The topic of the Datathon is Community Wellness; analyzing and promoting mental,",
                        "financial, social, spiritual, occupational, physical, intellectual, and environmental wellness in Hampton Roads.",
                        "These eight dimensions of wellness were developed by Dr. Margaret Swarbrick and are commonly cited and used in,",
                        "programs focusing on the wellness of individuals and communities. Our team is focusing on the six domains of physical health",
                        "identified by Dr. Swarbrick."))
               ),
               tabPanel("Objective",
                        HTML('<center><img src="Objective-Datathon-2023.png", width="1000", height="350"></center>'),
                        tags$br(),
                        tags$br(),
                        h4("Objective"),
                        p("To understand the Peggy-Swarbrick six domains of physical health at a fine-grained geographic level in Norfolk, VA we analyzed the CDC Places Data for Norfolk, VA. Our specific research question is:", tags$b("What behavior changes in the domains of Physical Health, in which census tracts, can improve health outcomes in Norfolk?"))
                        
               ),
               tabPanel("Data",
                        HTML('<center><img src="places-homepage-banner.jpg", width="900", height="150"></center>'),
                        tags$br(),
                        tags$br(),
                        h4("Data and Methods"),
                        p(paste("PLACES is a collaboration between CDC, the Robert Wood Johnson Foundation, and the CDC Foundation.", 
                        "PLACES provides health data for small areas across the country. This allows local health departments and jurisdictions,",
                        "regardless of population size and rurality, to better understand the burden and geographic distribution of health measures",
                        "in their areas and assist them in planning public health interventions. PLACES provides model-based, population-level analysis",
                        "and community estimates of health measures to census tracts in many cities across the United States.")),
                        p(paste("We have developed a web-based dashboard. For each census tract in Norfolk,",
                        "VA the dashboard shows percent of individuals engaging in healthy behaviors in each domain and how that compares to other census tracts.")),
                        br(),
                        p("Links to the CDC Places Project data, the methodology used to generate the data and studies related to the validation of the data are below."),
                        p(a("CDC Places Census Tract Level Data Release - August 25, 2023.", href="https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh", target="blank")),
                        p(a("Methodology for Generating CDC Places Project Census Tract Level Estimates.", href="https://www.cdc.gov/places/methodology/index.html", target="blank")),
                        p(a("Study Validating Census Tract Level Data Estimates.", href="https://www.cdc.gov/pcd/issues/2017/17_0281.htm", target="blank"))
                        
                        
               ),
               tabPanel("Addressing Data Limitations",
                        HTML('<center><img src="limitations.png", width="600", height="300"></center>'),
                        tags$br(),
                        tags$br(),
                        h4("Limitations"),
                        p(paste("Some of the domains, like healthy food choices, are not well represented in the CDC Places data set.",
                        "To address this limitation, we have created additional data sets.",
                        "They are viewable in the maps linked below")),
                        p(a("Maps with additional Norfolk, VA specific data for phyiscal health domains.", href="https://www.arcgis.com/apps/mapviewer/index.html?webmap=6457bb021ad1422d945ec7f635bc759a", target="blank"))
               ),
               tabPanel("Recommendations To Improve Health Outcomes",
                        HTML('<center><img src="recommendations.png", width="500", height="400"></center>'),
                        tags$br(),
                        tags$br(),
                        h4("Recommendations"),
                        p("For each domain we identified multiple changes in behaviors and practices that could specific improve health outcomes. These changes reflect behaviors in Norfolk, VA that are very strongly correlated (", tags$b("Correlation > 0.8 or Correlation < -0.8"), ") with specific improved health outcomes at the census-tract level.")
                       
               ),
               tabPanel("Who We Are",
                        HTML('<center><img src="WhoWeAre.png", width="700", height="700"></center>'),
                        h4("Team Members"),
                        p("The following researchers were involved in the completion of this work:"),
                        tabsetPanel(id = "Researchers",
                                    tabPanel("Kevin O'Brien", p(a("Kevin O'Brien", href="https://www.linkedin.com/in/kevin-o-brien-0334321bb/", target="blank")," is a Data Specialist at the Virginia Modeling, Analysis and Simulation Center (VMASC) at Old Dominion University. Kevin graduated with a computer science bachelor’s degree from Old Dominion University in May of 2021. Since being at VMASC, he has wored on the UNOS Lung Donor Agent-Based Model among other data science projects. His email address is", tags$b("kobrien@odu.edu."))),
                                    tabPanel("Virginia Zamponi", p(a("Virginia Zamponi", href="https://www.linkedin.com/in/virginia-zamponi-536052209/", target="blank")," is a Simulation Developer at the Virginia Modeling, Analysis and Simulation Center (VMASC) at Old Dominion University. She recently completed her MS in Modeling and Simulation at Old Dominion University. He also has a BS degree in Computer Science from Old Dominion University. Her email address is", tags$b("vzamponi@odu.edu."))),
                                    tabPanel("Jessica O'Brien", p(a("Jessica O'Brien", href="https://www.linkedin.com/in/jessica-cordner/", target="blank")," is an Applied Research Cloud Architect at the Virginia Modeling, Analysis and Simulation Center (VMASC) at Old Dominion University. She has a BS in Computer Engineering and a MS in Cybersecurity from Old Dominion University. She has extensive experience developing and deploying applications on Amazon's AWS Global Cloud Infrastructure. Her email address is", tags$b("jcordner@odu.edu."))),
                                    tabPanel("Ross Gore", p(a("Ross Gore", href="https://vmasc.org/staff-profiles/dr-ross-gore/", target="blank"), "is a Research Associate Professor at the Virginia Modeling, Analysis and Simulation Center (VMASC) at Old Dominion University. He holds a Doctorate of Philosophy (Ph.D.) and a Master's degree in Computer Science from the University of Virginia (UVA) and a Bachelor's degree in Computer Science from the University of Richmond (UR). His current work focuses on data science and predictive analytics. His email address is", tags$b("rgore@odu.edu"))),
                                    tabPanel("Christopher Lynch", p(a("Christopher Lynch", href="https://vmasc.org/staff-profiles/dr-christopher-lynch/", target="blank")," is a Research Assistant Professor and Certified Cloud Practitioner at the Virginia Modeling, Analysis and Simulation Center (VMASC) at Old Dominion University. He holds a Ph.D. in Modeling and Simulation (M&S) from Old Dominion University. He leads the Data Analytics Working Group at VMASC and works on developing new methods for analyzing simulation results. His email address is", tags$b("cjlynch@odu.edu."))), 
                                    tabPanel("Erik Jensen", p(a("Erik Jensen", href="https://scholar.google.com/citations?user=OHIeH5YAAAAJ&hl=en", target="blank")," is a Ph.D. candidate in the Department of Electrical and Computer Engineering at Old Dominion University, specializing in Modeling and Simulation Engineering. He has a GRA position and works on data-analytics projects with VMASC.  Within the field of parallel discrete event simulation, for his dissertation, he is researching methods to find event-set level parallelism, using data and scheduling dependencies. He received his MS degree in Electrical and Computer Engineering from Old Dominion University. His email address is", tags$b("ejjensen@odu.edu."))))
                        
               )
               
  )
)

