#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

## ui.R ##

navbarPage(
  'NYC Property', 
  theme = shinytheme("flatly"),
  
  # Interactive Map-----------------------------------------------------------------------------------
  tabPanel('New York AirBnb Map', 
           div( class='outer', tags$head(includeCSS('styles.css')),
                leafletOutput('mymap', width = '100%', height = '100%')),                  
           
           # Control Panel -----------------------------------------------------------------------------------
           
           # Control Panel Style: 
           absolutePanel(id = 'controls', 
                         class = 'panel panel-default', fixed = TRUE, draggable = TRUE, 
                         top = 60, left = 'auto', right = 30, bottom = 'auto', width = 500,  height = 'auto',
                         
                         # Control Panel-Choose Inputs:
                         
                         fluidRow( column(6, selectInput(inputId = 'element', 
                                                         label = h4('Select'),
                                                         choices = element, 
                                                         selected = 'Volume')
                         ),
                         column(6, selectInput(inputId = 'borough', 
                                               label = h4('Select Borough'),
                                               choices = borough, 
                                               selected = 'All')
                         )
                         ),
                         br(),
                         
                         # Control Panel- Plot Output:
                         plotlyOutput("Donut", height = 270), # pie chart for room_type breakdown
                         
                         plotlyOutput("zp", height = 270)   # bar chart for price by zipcode
           )
  ),# Tab panel end
  
  
  # Tab 2  NYC Borougnwise bubble Map---------------------------------------------------------------------
  tabPanel('New York Zillow Map', 
           div( class='outer', tags$head(includeCSS('styles.css')),
                leafletOutput('mymap2', width = '100%', height = '100%')),
           
          # Control Panel -----------------------------------------------------------------------------------
          # Control Panel Style: 
          absolutePanel(id = 'controls', 
                        class = 'panel panel-default', fixed = TRUE, draggable = TRUE, 
                        top = 60, left = 'auto', right = 30, bottom = 'auto', width = 600,  height = 'auto',
          
                        # Control Panel-Choose Inputs:
            
                        fluidRow( column(6, sliderInput(inputId = "time", 
                                                        label = h4("Select Date Range"),
                                                        min = min(price$time), max = max(price$time),
                                                        value = c(min(price$time),max(price$time)),timeFormat="%b %Y",step=10)
                                        ),
                                  column(6, selectInput(inputId = 'county', 
                                                        label = h4('Select Borough'),
                                                        choices = borough, 
                                                        selected = 'All')
                                        )
                                  ),
                        br(),
          
                        # Control Panel- Plot Output:
                       
                        plotlyOutput("Time", height = 500)   # line chart for price by zipcode and date
          
          )
),# Tab2 panel end

  
  
  # Tab 3  Data Table Explorer--------------------------------------------------------------------
  tabPanel("Profitability Analysis",
           column(6,
                  h3("Rank for Individual Metric"),
                  DT::dataTableOutput("table")),
           column(6,
                  h3("Breakeven Period vs. Total Number of Reviews"),
                  plotlyOutput('be'))
)
  
) # End



