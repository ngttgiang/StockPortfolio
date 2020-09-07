#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Make a Portfolio"),

    

    # Sidebar 
    sidebarLayout(
        sidebarPanel(

            # text input for sector1
            # textInput(inputId = "sector1",
            #           label = "Choose sector",
            #           value = "Finance",
            #           placeholder = 'Enter a sector'
            # ),
            
            #select sector
            selectInput(inputId = 'sector1',
                        label = 'Select sector',
                        choices = c('Finance'= 'Finance', 
                                    'Technology'='Technology', 
                                    'Healthcare'='Healthcare'),
                        multiple = FALSE,
                        selected = 'Finance'
            ),
            
            # text input for sector2
         
            
            # text input for s_date
            textInput(inputId = "sdate",
                      label = "Start date for lookback period",
                      value = "2018-01-01",
                      placeholder = 'Enter a date yyyy-mm-dd'
            ),
            
           
            # input number of month
            numericInput(inputId = "nmonth",
                      label = "Number of month from the lookback start date",
                      value = 12,
                      min = 1,
                      max = 12,
                      step = 1
            ),
            
            # input number of stock to pick
            numericInput(inputId = "N",
                         label = "Number of stock to pick",
                         value = 2,
                         min = 1,
                         max = 5,
                         step = 1
            ),
            
            # input number of trading volume
            numericInput(inputId = "vol",
                         label = "Threshold of trading volume",
                         value = 1e6
            ),
            
            # input number of market cap
            numericInput(inputId = "cap",
                         label = "Threshold of market cap",
                         value = 1e9
            ),
            
           actionButton(
               inputId = 'submit_loc',
               label = "Submit"
           ),
            
            # text input for s_date portfolio
            textInput(inputId = "p_sdate",
                      label = "Start date for portfolio",
                      value = "2018-01-01",
                      placeholder = 'Enter a date yyyy-mm-dd'
            ),
            
            # input number of month
            numericInput(inputId = "p_nmonth",
                         label = "Number of month from the portfolio start date",
                         value = 12,
                         min = 1,
                         max = 12,
                         step = 1
            ),
            
            # input number of rebalance frequency
            numericInput(inputId = "rebalance",
                         label = "Rebalance frequency",
                         value = 3,
                         min = 1,
                         max = 12,
                         step = 1
            ),
            
            # input number of initial quantity
            numericInput(inputId = "init_quantity",
                         label = "Initial Quantity",
                         value = 10
            ),
            
            # Risk free rate
            numericInput(inputId = "rf",
                         label = "Risk-free rate",
                         value = 0.06
            ),
        ),
            

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("stocks"),
            
            plotOutput("portfolioPlot")
        )
    )
))

