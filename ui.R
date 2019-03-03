##############################################
#
# UI code for educational application about linear correlation
#
# Simon Benateau 09.10.2014
#           
##############################################

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Linear correlation"),
  
    splitLayout(
      #Input widjets
      radioButtons("select", label = "Select an example", 
                         choices = list("Example 1" = 1, "Example 2" = 2,
                                        "Example 3" = 3,"Example 4" = 4,"Example 5" = 5), selected = 1),
      sliderInput("corr", label = "Correlation coefficient",
                  min = -1, max = 1, value = 0.9, step = 0.05)
),
      plotOutput("CorrelationPlot")
    
  )
)

