vars <- c(
  "Drawing" = "drawing",
  "Input from .zip boundary shapefile" = "shapefile",
  "Select from existing boundary" = "selecting"
)

varsmcda<-c(
  "10,000"=10000,
  "100,000"=100000
)

varsportfolio<- c(
  "Drawing" = "drawing",
  "Input from .zip boundary shapefile" = "shapefile"
)

varswsb<- c(
  "Single watershed" = "onewatershed",
  "Multiple watersheds" = "multwatershed"
)

vars1 <-c(
#  "State Costal Zone" = "state",
#  "County Coastal Zone" ="County",
  "Watershed Coastal Zone" ="Watershed"
)

vars2<-c(
  "One project mode" = "oneproject",
  "Multiple projects mode" = "MCDA"
  )

vars3 <- c(
  "Drawing" = "drawing",
  "Select from a HUC-12 watershed" = "watershed",
  "Input from .zip boundary shapefile" = "shapefile"#,
  #"Select from existing boundary" = "selecting"
)

vars4 <- c(
  "Select by HUC 12 Watershed name" = "name",
  "Select by HUC 12 Watershed ID" = "id",
  "Select by HUC 12 Watershed boundary"="boundary"
)

vars5<-c(
  "The higher the better" = "positive",
  "The lower the better" = "negative"
)

vars6 <- c(
  "Select by HUC 12 Watershed name" = "name",
  "Select by HUC 12 Watershed ID" = "id",
  "Select by HUC 12 Watershed boundary"="boundary"
)


tagList(
        tags$head(
          useShinyjs()
        ),
        navbarPage( title= "Gulf Coast Conservation Prioritization Tool", id="nav",
                   tabPanel("Area of Interest",
                            div(class="outer",
                                tags$head(
                                  includeCSS("styles.css"),
                                  extendShinyjs(text = jscode, functions = c("refresh","pageCol"))
                                ),
                                scr,
                                leafletOutput("map", width="100%", height="100%"),
                                hidden(
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 400, height = "auto",
                                              conditionalPanel("output.mode =='mcda'",
                                              h2("Specify Area of Interests"),
                                              selectInput("way", "Input", vars),
                                              conditionalPanel("input.way == 'drawing'",
                                                               
                                                                 numericInput("threshold", "Number of Projects:",3,max=10),
                                                                 
                                                                 
                                                              
                                                               h5("Project(s) Left to Enter:"),
                                                               splitLayout(
                                                                 verbatimTextOutput("value"),
                                                                 actionButton("learnmore",label="",icon = icon("exclamation-circle", "fa-2x"))
                                                               ),
                                                               
                                                               conditionalPanel("output.drawingpolygon < 1",
                                                                                textInput("text", label = h5("Name your project here:"), value = "Area of Interest 1"),
                                                                                actionButton("button", "Finalize this Area of Interest",style="color:#0000FF;")
                                                               ),
                                                               tags$br(),
                                                               tags$br(),
                                                               conditionalPanel("output.value==0",
                                                                                actionButton("finish","Finish",style="color:#0000FF;")
                                                               )
                                              ),
                                              conditionalPanel("input.way == 'shapefile'",
                                                               # Only prompt for threshold when coloring or sizing by superzip
                                                               fileInput('file1', 'Choose .zip boundary file',accept=c('.zip')),
                                                               h5("Number of area of interests imported:"),
                                                               verbatimTextOutput("value_import"),
                                                               tags$br(),
                                                               actionButton("trail","Finish",style="color:#0000FF;")
                                              ), 
                                              conditionalPanel("input.way == 'selecting'",
                                                               selectInput("Scale","Select a scale",vars1),
                                                               numericInput("threshold_watershed", "Number of Watersheds:",3,max=10,min = 2),
                                                               
                                                               selectInput("way4","Watershed Selection Options",vars6),
                                                               conditionalPanel("input.way4=='name'",
                                                                                uiOutput("watershed_uimcda")
                                                               ),
                                                               conditionalPanel("input.way4=='id'",
                                                                                uiOutput("watershed_uimcda_id")
                                                               ),
                                                               conditionalPanel("input.way4=='boundary'"
                                                                                
                                                               ),
                                                               actionButton("selectingbutton","Finish",style="color:#0000FF;")
                                              )
                                              ),
                                              conditionalPanel("output.mode =='os'",
                                                               h2("Specify Area of Interest"),
                                                               selectInput("way2", "Input", vars3),
                                                               conditionalPanel("input.way2 =='drawing'",
                                                                                textInput("text1", label = h5("Name your project here:"), value = "Area of Interest 1"),
                                                                                splitLayout(cellWidths = c("75%", "25%"),
                                                                                actionButton("osfinish","Finalize this Area of Interest",style="color:#0000FF;"),
                                                                                actionButton("learnmoreos",label="",icon = icon("exclamation-circle", "fa-2x"))
                                                                                )
                                                                                ),
                                                               conditionalPanel("input.way2 == 'shapefile'",
                                                                                fileInput('file2', 'Choose shapefile',accept=c('.zip')),
                                                                                tags$br(),
                                                                                tags$br(),
                                                                                actionButton("ostrail","Finish",style="color:#0000FF;")
                                                                                
                                                                                ),
                                                               conditionalPanel("input.way2 == 'watershed'",
                                                                                selectInput("way3","Watershed Selection Options",vars4),
                                                                                conditionalPanel("input.way3=='name'",
                                                                                                 selectizeInput("watershed_os_name","Select a watershed",choice),
                                                                                                 actionButton("oswatershedname","Finish",style="color:#0000FF;")
                                                                                ),
                                                                                conditionalPanel("input.way3=='id'",
                                                                                                 selectizeInput("watershed_os_id","Select a watershed",choiceid),
                                                                                                 actionButton("oswatershedid","Finish",style="color:#0000FF;")
                                                                                ),
                                                                                conditionalPanel("input.way3=='boundary'",
                                                                                                 actionButton("oswatershedboundry","Finish",style="color:#0000FF;")
                                                                                )
                                                                                )
                                                               ),
                                              conditionalPanel("output.mode =='portfolio'",
                                                               h2("Specify Area of Interests"),
                                                               selectInput("waysportfolio", "Input", varsportfolio),
                                                               conditionalPanel("input.waysportfolio == 'drawing'",
                                                                                numericInput("threshold_portfolio", "Number of Projects:",4,max=10),
                                                                                h5("Project(s) Left to Enter:"),
                                                                                verbatimTextOutput("value_portfolio"),
                                                                                conditionalPanel("output.drawingpolygon < 1",
                                                                                                 textInput("text_portfolio", label = h5("Name your project here:"), value = "Area of Interest 1"),
                                                                                                 actionButton("button_portfolio", "Finalize this Area of Interest")
                                                                                ),
                                                                                tags$br(),
                                                                                tags$br(),
                                                                                conditionalPanel("output.value_portfolio==0",
                                                                                                 actionButton("finish_portfolio","Finish"))
                                                               ),
                                                               conditionalPanel("input.waysportfolio == 'shapefile'",
                                                                                fileInput('file1_portfolio', 'Choose .zip boundary file',accept=c('.zip')),
                                                                                tags$br(),
                                                                                h5("Number of area of interests imported:"),
                                                                                verbatimTextOutput("value_import_portfolio"),
                                                                                tags$br(),
                                                                                actionButton("trail_portfolio","Finish",style="color:#0000FF;")
                                                                                )
                                                               ),
                                              
                                              
                                              tags$br(),
                                              
                                              actionButton("refresh", "Start over"),
                                              #tags$br(),
                                              #style = "z-index: 1000;"
                                ))
                               
                                
                            )),
                   tabPanel("Assessment",
                            value = "MCDA",
                            tabsetPanel(type = "tabs",
                                        id = "tabs",
                                        tabPanel("Single project Data Overview",
                                                 value="osdataoverview", id = "osdataoverview",
                                                 fluidRow(
                                                   column(9,
                                                          tabsetPanel(id="viewdataos",
                                                            tabPanel("Data Summary",
                                                                     value = "osdatasummary",
                                                                     DT::dataTableOutput("showing_matrix_os") 
                                                              
                                                            ),
                                                            tabPanel("Rename Area of Interest",value = "renameos",
                                                                      br(),
                                                                      #textInput("osrename",value = proplist_os,label = proplist_os),
                                                                      uiOutput("osnamechange"),
                                                                      actionButton("osconfirmname","Make Changes to the Names")
                                                                     
                                                            ),
                                                            tabPanel("Refine Area of Interest",value = "redefineos",
                                                                     br(),
                                                                     leafletOutput("mapresultos",width = "100%", height = 800)
                                                                     )
                                                          )),
                                                   column(3,
                                                          br(),
                                                          br(),
                                                          #downloadButton("osdownload","Report"),
                                                          downloadButton("osdownload1","Detailed Report"),
                                                          br(),
                                                          br(),
                                                          downloadButton("osspdownload","Spatial footprint"),
                                                          br(),
                                                          br(),
                                                          downloadButton("downloadDataos","Export Raw Datatable"),
                                                          hr(),
                                                          actionButton("osadvance","Advanced Options"),
                                                          hr(),
                                                          div(id='advancedoptionsos',
                                                          br(),
                                                          actionButton("osrename","Rename Area of Interest"),
                                                          br(),
                                                          br(),
                                                          actionButton("osgotodeselect","Refine Area of Interest"),
                                                          actionButton("osupdatetable","Deselect highlighted Hexagons"),
                                                          br(),
                                                          br(),
                                                          actionButton("showsupportlayeros","Turn on all the supporting layers"),
                                                          br(),
                                                          hr()
                                                          ),
                                                          leafletOutput("mapresult4",height = 250) 
                                                          )
                                                 )
                                                 ),
                                        
                                        tabPanel("Portfolio Data Overview",
                                                 value="portfoliooverview",id="portfoliooverview",
                                                 fluidRow(
                                                   column(9,
                                                          tabsetPanel(
                                                            tabPanel("Data Summary",
                                                                     
                                                                     value = "portfoliodatasummary",
                                                                     DT::dataTableOutput("showing_matrix_portfolio_goal")          
                                                           ),
                                                           tabPanel("Detail View",
                                                                    DT::dataTableOutput("showing_matrix_portfolio") 
                                                                    )
                                                          )),
                                                   column(3,
                                                          br(),
                                                          br(),
                                                          #downloadButton("portfoliodownload","Report"),
                                                          downloadButton("portfoliospdownload","Spatial footprint"),
                                                          br(),
                                                          hr(),
                                                          plotlyOutput("barportfolio"),
                                                          br(),
                                                          br(),
                                                          leafletOutput("mapresult_portfolio",height = 250) 
                                                          )
                                                 )
                                        ),
                                        tabPanel("Data Overview",
                                                 value= "dataoverview",id = "dataoverview",
                                                 fluidRow(
                                                   column(9,
                                                          tabsetPanel(id="viewdata",
                                                            tabPanel("Raw Data",value = "rawdata",
                                                                  # hr(),           
                                                                  DT::dataTableOutput("showing_matrix") ,                                              
                                                            ),
                                                            tabPanel("Scaled Data",value = "scaledata",                                                                     
                                                                  DT::dataTableOutput("showing_matrix_scaled") 
                                                            ),
                                                            tabPanel("Deselect Hexagons",value = "deselect",
                                                                     br(),
                                                                     leafletOutput("mapresult",width = "100%", height = 800) 
                                                                     ),
                                                            #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                            #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                            #                })")),
                                                            tabPanel("Utility Function Options",value = "utility",
                                                                     fluidRow(
                                                                       column(3,
                                                                              selectInput("utility_urban",label = "Threat of Urbanization",vars5,selected = "positive"),
                                                                              selectInput("utility_303D",label = "303D Impaired Watershed",vars5,selected = "positive"),
                                                                              selectInput("utility_light",label = "Light Pollution Index",vars5,selected = "positive"),
                                                                              actionButton("finish_utility",label = "Finalize Utility Function Input")
                                                                              ),
                                                                       column(9,
                                                                              DT::dataTableOutput("showing_matrix2") 
                                                                              )                                                                     
                                                                     )                                                                     
                                                            ),
                                                            tabPanel("Rename Area of Interest",value = "rename",
                                                                     br(),
                                                                     fluidRow(
                                                                       column(6,                                                                              
                                                                              actionButton("confirmname","Make Changes to the Names")
                                                                       )  
                                                                     )        
                                                            ),
                                                            tabPanel("Adding Attributes",value = "addattr",
                                                                     fluidRow(
                                                                       column(4,
                                                                              textInput("namenewadded","Name of the Attributes",
                                                                                          value = "New attribute 1"),
                                                                              selectInput("goalnewadded", "Pick a RESTORE Goal:",
                                                                                          c("Habitat" = "HA","Water Quality & Quantity" = "WQ",
                                                                                          "Living Coastal & Marine Resources" = "LCMR", 
                                                                                          "Community Resilience" = "CL","Gulf Economy" = "EC")),
                                                                              actionButton("finishaddweight","Add This Attribute"),
                                                                              actionButton("removeaddweight","Remove Selected Attribute")                                                                              
                                                                              ),
                                                                       column(8,
                                                                              DT::dataTableOutput("addattrtable"))
                                                                     )
                                                                     )
                                                          )                                                          
                                                   ),
                                                   column(3,
                                                          br(),
                                                          actionButton("showdefault","Show Default Result",style="color:#0000FF;"),
                                                          br(),
                                                          hr(),
                                                          actionButton("runsmaa", "Simulate Random Weights"),
                                                          br(),
                                                          br(),
                                                          actionButton("startweight", "Choose My Own Weights"),
                                                          
                                                          hr(),
                                                          actionButton("advance","Advanced Options"),
                                                          hr(),
                                                          div(id='advancedoptions',
                                                          actionButton("gotorawtable","View Data Table"),
                                                          br(),
                                                          br(),
                                                          
                                                          actionButton("gotodeselect","Refine Area of Interest"),
                                                          actionButton("updatetable","Deselect highlighted Hexagons"),
                                                          br(),
                                                          br(),
                                                          
                                                          actionButton("gotoutility","Customize Utility Function"),
                                                          #actionButton("updateutility","Change Utility Functions"),
                                                          br(),
                                                          br(),
                                                          
                                                          actionButton("addweight","Add Attribute"),
                                                          br(), 
                                                          br(),
                                                      
                                                          actionButton("renameproject","Rename Area of Interest"),
                                                          
                                                          br(),
                                                          br(),
                                                          actionButton("adjustmcdanumbers","Adjust the numbers of iterations run by MCDA model"),
                                                          selectInput("numMCDA"," ",varsmcda),
                                                          br(),
                                                          br(),
                                                          actionButton("showsupportlayer","Turn on all the supporting layers"),
                                                          br(),
                                                          hr(),
                                                          ),
                                                          downloadButton("download2","Detailed Report V3"),
                                                          br(),
                                                          br(),
                                                          downloadButton("downloadData","Export Raw Datatable"),
                                                          br(),
                                                          hr(),
                                                          leafletOutput("mapresult5",height = 250) 
                                                   ))
                                        ),
                                        
                                        tabPanel("Goals",
                                                 value = "goalweight",
                                                 bsTooltip("goalweight", "The default weighting for each of the five goals is 20%. However, a user may adjust the weighting of the goals to reflect their preferred values. The only weighting limitation is that the total weight of all goals must sum to 100%.",
                                                           "right", options = list(container = "body")),
                                                 fluidRow(
                                                   
                                                   column(6,
                                                          br(),
                                                          sidebarPanel( width = 10,
                                                          br(),
                                                          splitLayout( 
                                                            sliderInput("habitat", "Habitat: ", min = 0, max = 100, value = 20,post ="%",step = 5,width = '100%'),
                                                            numericInput("habitat1"," ", min = 0, max = 100, value = 20,step = 5,width = '40%')
                                                          ),
                                                          splitLayout( 
                                                            sliderInput("water", "Water Quality & Quantity: ",  min = 0, max = 100, value = 20,post ="%",step = 5,width = '100%'),
                                                            numericInput("water1", " ", min = 0, max = 100, value = 20,step = 5,width = '40%')
                                                          ),
                                                          splitLayout( 
                                                            sliderInput("species", "Living Coastal & Marine Resources: ", min = 0, max = 100, value = 20,post ="%",step = 5,width = '100%'),
                                                            numericInput("species1"," ", min = 0, max = 100, value = 20,step = 5,width = '40%')
                                                          ),
                                                          splitLayout( 
                                                            sliderInput("resilience", "Community Resilience: ", min = 0, max = 100, value = 20,post ="%",step = 5,width = '100%'),
                                                            numericInput("resilience1"," ", min = 0, max = 100, value = 20,step = 5,width = '40%')
                                                          ),
                                                          splitLayout( 
                                                            sliderInput("economy", "Gulf Economy: ", min = 0, max = 100, value = 20,post ="%",step = 5,width = '100%'),
                                                            numericInput("economy1"," ", min = 0, max = 100, value = 20,step = 5,width = '40%')
                                                          )
                                                   )
                                                          ),
                                                   column(6,
                                                          br(),
                                                          br(),
                                                          tableOutput("GW_table")),
                                                   br(),
                                                   br(),
                                                   p("The Weights should sum to 100"),
                                                   br(),
                                                   br(),
                                                   actionButton("goaldone", "Finalize Weights for Goals")
                                                   
                                                 )
                                        ), 
                                        
                                        tabPanel("Measures",                            
                                                 value = "PAweight",
                                                 tabsetPanel(type="tabs",
                                                             id = "tabsPA",
                                                             tabPanel("Habitat",
                                                                      value = "HA",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("hab_PA_measures_Table") ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #                })")),
                                                                        column(6,
                                                                               br(),
                                                                               actionButton("HAmove", "Next")))),
                                                             tabPanel("Water Quality & Quantity",
                                                                      value = "WQ",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("wq_PA_measures_Table") ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #                })")),
                                                                        column(6,
                                                                               br(),
                                                                               actionButton("WQmove", "Next")))),
                                                             tabPanel("Living Coastal & Marine Resources",
                                                                      value = "LC",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("lcmr_PA_measures_Table") ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #                })")),
                                                                        column(6,
                                                                               br(),
                                                                               actionButton("LCmove", "Next")))),
                                                             tabPanel("Community Resilience",
                                                                      value = "CL",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("commres_PA_measures_Table") ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #               Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #               })")),
                                                                        column(6,
                                                                               br(),
                                                                               actionButton("CLmove", "Next")))),
                                                             tabPanel("Gulf Economy",
                                                                      value = "EC",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("gulfecon_PA_measures_Table") ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #                })")),
                                                                        column(6, 
                                                                               br(),
                                                                               actionButton("ECmove", "Next")))),
                                                             tabPanel("User Added Attributes",
                                                                      value = "UA",
                                                                      fluidRow(
                                                                        column(6,
                                                                               br(),
                                                                               DT::dataTableOutput("UserAdded_PA_measures_Table") 
                                                                               ),
                                                                        #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                                                        #                Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                                                        #                })")),
                                                                        column(6,
                                                                               br(),
                                                                               actionButton("UAmove", "Next")))),
                                                             tabPanel("Weights Review",
                                                                      fluidRow(column(5,
                                                                                      DT::dataTableOutput("weights_review") ),
                                                                               column(5,
                                                                                      tableOutput("weights_review_ua")),
                                                                               column(2, 
                                                                                      br(),
                                                                                      actionButton("Weightsdone", "Finalize weights",style="color:#0000FF;")
                                                                                      )))
                                                 )
                                        )
                            )
                   ),
                   tabPanel("Results",value = "Result",
                            tabsetPanel(id = "tabsResult",
                                        type = "tabs",
                                        tabPanel("Default Result",
                                                 value="defaultweight",
                                                 fluidRow(
                                                   column(5,style='padding:10px;',
                                                          uiOutput("defaultresult2"),
                                                          em("The table above shows the raw score for each RESTORE goal aggregated from weighted Priority Attributes."),
                                                          br()
                                                   )),
                                                 hr(),
                                                 fluidRow(
                                                   column(6,style='padding:10px;',
                                                          uiOutput("defaultresult4")
                                                          
                                                   ),
                                                   column(5, offset = 1,
                                                          br(),
                                                          actionButton("backtomain","Back to main menu", style = "color:#0000FF;"),
                                                          br(),
                                                          br(),
                                                          actionButton("gotodefineweights","If you would like to alter weights in the comparison..."),
                                                          hr(),
                                                          absolutePanel(
                                                            leafletOutput("mapresult6", width=600, height=350),
                                                            top= 140,width = "80%"
                                                          )
                                                          )
                                                   )
                                                   
                                                 
                                        ),
                                        tabPanel("Simulation Model Output",
                                                 value = "SMAA_result",
                                                 fluidRow(column(6,
                                                                 wellPanel(
                                                                 br(),
                                                                 uiOutput("resultpresent1"),
                                                                 uiOutput("resultpie")),
                                                                 br(),
                                                                 uiOutput("textresult")
                                                 ),
                                                 column(6,
                                                        uiOutput("resultpresent3"),
                                                        br(),
                                                        downloadButton("explaincentral","How can I use Central Weights?"),
                                                        
                                                        br(),br(),
                                                        
                                                        actionButton("backtomain1","Back to main menu", style = "color:#0000FF;"),
                                                        br(),br(),
                                                        downloadButton("report", "Generate Detailed report"),
                                                        br(),br(),
                                                        downloadButton("report2", "Download Spatial Footprint")
                                                        )
                                                 
                                                 )
                                        ),
                                        tabPanel("User Defined Weights",
                                                 value = "Weights_result",
                                                 fluidRow(column(5,style='padding:10px;',
                                                                 uiOutput("resultpresent2"),
                                                                 em("The table above shows the raw score for each RESTORE goal aggregated from weighted Priority Attributes."),
                                                
                                                 )),
                                                 hr(),
                                                 fluidRow(
                                                 column(6,style='padding:10px;',
                                                        uiOutput("resultpresent4")
                                                        ),
                                                 column(5,offset = 1,
                                                        downloadButton("report1", "Generate Detailed report"),
                                                        br(),br(),
                                                        downloadButton("report3", "Download Spatial Footprint"),
                                                        hr(),
                                                        absolutePanel(
                                                          leafletOutput("mapresult1", width=600, height=350),
                                                          top= 130,width = 600
                                                        )
                                                 )
                                                 )
                                                
                                        )
                            )
                            
                   ),
                   tabPanel("Help",
                            tabsetPanel(id="tabsAbout",
                                        tabPanel("Contact Us",
                                                 column(8,
                                                        h5(strong("For more information please contact our Conservation Applications Specialist: ")),
                                                        h6(tags$b("Amanda Sesser")),
                                                        h6("Strategic Conservation Assessment Project"),
                                                        HTML(paste0("Email: ",'<a href="mailto:scaprojectgulf@gmail.com"> scaprojectgulf@gmail.com</a>')),
                                                        hr(),
                                                        h5(strong("Or one of our project's Principal Investigators: ")),
                                                        h6(tags$b("Kristine Evans")),
                                                        h6("Co-Director of the Quantitative Ecology and Spatial Technologies Lab (QuEST) Lab"),
                                                        h6("Mississippi State University"),
                                                        h6("Department of Wildlife Fisheries and Aquaculture"),
                                                        HTML(paste0("Email: ",'<a href="mailto:kristine.evans@msstate.edu"> kristine.evans@msstate.edu</a>')),
                                                        hr()
                                                        
                                                 )
                                                 #,
                                                 #column(6,
                                                 #                tags$style("#logo3 {vertical-align:top;margin-top:550px;}"),
                                                 #                imageOutput("logo3"))
                                                 )
                                        )
                                                 
                            )
                   
                   
                   )
        )