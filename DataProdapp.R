library(shiny)
library(dplyr)    

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Interactive single linaer models for the Mtcars Dataset"),
   
   # Sidebar with a selectInput for selection of parameters
   sidebarLayout(
      sidebarPanel(
        selectInput("variable", "Choose a Parameter to do a linear model with", 
                    c("Cylinders" = "cyl", "Transmission" = "am", "Gears" = "gear","Displacement"="disp","Horse Power"="hp","Rear Axle Ratio"="drat","Weight"="wt","V/S"="vs","Transmission"="am"))
        ),
      
      # Show a plot, interceptand and coeficient of the linear model
      mainPanel(
        plotOutput("firstPlot"),
        h4("Intercept for the linear model:"),
        textOutput("a"),
        h4("Coefficient for the linear model:"),
        textOutput("b"),
        plotOutput("secPlot")
      )
   )
)

# Define server logic required to draw plots
server <- function(input, output) {
   
  
  df <- reactive({ 
    df <- mtcars %>% select(mpg, input$variable)
  })
  
  output$secPlot <- renderPlot({ 
    dfb <- df() 
    plot(lm(dfb[, 1]~ dfb[, 2])  ) 
  })
  
  output$firstPlot <- renderPlot({ 
    dfb <- df() 
    plot(dfb[, 1]~ dfb[, 2],main = "Plot of   mpg ~ chosenParameter", xlab= input$variable,ylab="mpg")   
    abline(lm(dfb[, 1]~ dfb[, 2]), lwd=1, col="red")
    output$a<-renderText(   summary(lm( dfb[, 1]~ dfb[, 2]))$coefficients[1,1]    )
    output$b<-renderText(   summary(lm( dfb[, 1]~ dfb[, 2]))$coefficients[2,1]    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

