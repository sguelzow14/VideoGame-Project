
library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MetaCritic PS4 Games"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Top 100", "Past 90 Days", "Full"))
      ),
      
      
   
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      
   )),
  mainPanel(
    plotOutput("NextPlot")
  ),
  mainPanel(
    plotOutput("NNextPlot")
  ),
  mainPanel(
    plotOutput("NNNextPlot")
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     ds <- All
     if (input$dataset == "Full") {
       ds <- All
     }
     if (input$dataset == "Top 100") {
       ds <- TOPPS4
     }
     if (input$dataset == "Past 90 Days") {
       ds <- Final
     }
      ggplot(data=ds)+
      geom_point(mapping=aes(x=MetaScore,y=User.Score))+
        geom_smooth(aes(x=MetaScore,y=User.Score))

   })
   output$NextPlot <- renderPlot({
     ds <- All
     if (input$dataset == "Full") {
       ds <- All
     }
     if (input$dataset == "Top 100") {
       ds <- TOPPS4
     }
     if (input$dataset == "Past 90 Days") {
       ds <- Final
     }
     ggplot(data=ds)+
       geom_point(mapping=aes(x=Critics,y=MetaScore))+
       geom_smooth(aes(x=Critics,y=MetaScore))
     
   })
   output$NNextPlot <- renderPlot({
     ds <- All
     if (input$dataset == "Full") {
       ds <- All
     }
     if (input$dataset == "Top 100") {
       ds <- TOPPS4
     }
     if (input$dataset == "Past 90 Days") {
       ds <- Final
     }
     ggplot(data=ds)+
       geom_point(aes(x=Number.of.Reviews,y=User.Score))+
       geom_smooth(aes(x=Number.of.Reviews,y=User.Score))
     
   })
   output$NNNextPlot <- renderPlot({
     ds <- All
     if (input$dataset == "Full") {
       ds <- All
     }
     if (input$dataset == "Top 100") {
       ds <- TOPPS4
     }
     if (input$dataset == "Past 90 Days") {
       ds <- Final
     }
     ggplot(data=ds)+
       geom_point(aes(x=Number.of.Reviews,y=Critics))+#Correlation
       geom_smooth(aes(x=Number.of.Reviews,y=Critics))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

