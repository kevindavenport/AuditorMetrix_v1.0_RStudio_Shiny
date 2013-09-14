shinyUI(pageWithSidebar(
  headerPanel(HTML(' <div id="header_logo"></div>'),windowTitle = "AuditorMetrix v.60b"),   
  
  sidebarPanel(
    wellPanel(
      selectInput("client_select", "Client:", data_sets),
      uiOutput("serviceControls"),
      uiOutput("roundControls"),
      # Counts panel div
      div(
        div(
        div("Total Audits:", id ="side_totalaudits_label"),
        div(textOutput("auditsCount2"), id = "side_totalaudits_output"),
        id = "side_totalaudits_div"
        ),
        div(
        div("Unique Specialist:", id ="side_uniquespecialist_label"),
        div(textOutput("specialistCount2"),id = "side_uniquespecialist_output"),
        id = "side_uniquespecialist_div"
        ),
        id ="side_countsdiv")
    ),
    
    conditionalPanel("input.tabs1 == 'Density' ",
                     wellPanel( 
                       numericInput("obs", "Number of observations to view:", 5500)
                     )
    ),
    
    conditionalPanel(condition = " input.tabs1 != 'Control Chart' & input.tabs1 != 'Summary' ",
                    wellPanel(
                      selectInput("graph_variable.Y", "Graph Variable Y:",
                                  list("Score" = "Score", "Audit.Duration" = "Audit.Duration")),
                      selectInput("graph_variable.X", "Graph Variable X:",
                                  list("Round" = "Round", "Tenure" = "Tenure","State" = "State")),
                      checkboxInput("outliers", "Show outliers", FALSE)
                      )
                    ),
    
    conditionalPanel("input.tabs1 == 'Control Chart' ",
                     wellPanel(selectInput("cc.colorby", "Color Points by:",
                                           list("Tenure" = "tenure_mode", "Day Part" = "daypart_mode")))
                     ),
    
    wellPanel(checkboxInput(inputId = "pageable", label = "Paginate Summary Table", value = F),
              #numericInput(inputId = "pagesize", label = "Observations per page",40),
              HTML('<br>'),
              downloadButton('download.summary', 'Export summary as .csv', 
                             class = "btn btn-primary")),
    
    HTML('<footer>AuditorMetrix v.60b © 2013 kldavenport.com</footer>')
  ),
    
  mainPanel(
    tabsetPanel(id = "tabs1",
                tabPanel("Summary",
                        div(div(textOutput("auditsCount")),
                            div(textOutput("specialistCount")),
                            id = "countsdiv"),
                        div(h4("Score"), 
                            tableOutput("ScoreDescribe"),
                            id = "score_describe", class = "alert"),
                        div(h4("Duration"),
                            tableOutput("DurationDescribe"),
                            class ='alert alert-success', id = "duration_describe"),
                        div(h4(htmlOutput("specialistPlyr")), id = 'specialistPlyr')), 
                tabPanel("Linear Model", h4("Duration vs Score"),plotOutput("lmPlot"),
                         h4("Linear Models from Inputs"),verbatimTextOutput("LM")),
                tabPanel("Control Chart", h4("Control Chart"), plotOutput("ccPlot",width="800px",height="2000px")),
                tabPanel("Density", h4("Density"), plotOutput("densityPlot")),
                tabPanel("Boxplot", h4("Boxplot"), plotOutput("boxPlot")),
                tabPanel("Resources", h4("Resources"),includeHTML("www/resources.html"))
                                            
                
                ),
    # CSS mainPanel overides 
    tags$link(rel = "stylesheet",type = 'text/css', href = "style.css")
    )
  )
)