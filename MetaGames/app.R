
library(shiny)
All<- readRDS("AllTitle")
Final<- readRDS("Final")
TOPPS4<-readRDS("TOPPS4")
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
     h4("For Top 100 All Time:"),
     h6("There is a consistantly high UserScore given to each of the games in the Top 100 games of all time. It looks like the Critics and Users agree somewhat because there is a slight positive correlation between the two plots. However, this relationship is not strong enough to definitively say that a higher MetaScore will cause Users to rate the game high as well."),
     h4("For Past 90 Days:"),
     h6("In the past 90 days, UserScore and MetaScore again seem to be correlated. As one increases, the other does as well. However, it is intresting to note that the games that are lowest rated by critics seemed to be loved by the Users, which creates an intresting dichotomy that if your game is bad enough, then it can be loved by the players."),
     h4("For Full:"),
     h6("Overall, we see the clear positive correlation that UserScore and MetaScore have together, most of the time, the critics and Users of the game agree whether a game is quality or not.")
   ),
   
  mainPanel(
    plotOutput("NextPlot")
  ),
  mainPanel(
    h4("For Top 100 All Time:"),
    h6("The trend with the Top 100 games is that the more people to review the game, the higher your MetaScore will be. This trend seems to start when you cross the 35 Critic threshold"),
    h4("For Past 90 Days:"),
    h6("In the past 90 days, The number of reviewers doesn not seem to change the rating that was given on MetaCritic.com. Most of the games do not have many reviews as they are quite new, however, they seem to hover around the same MetaScore which means that there have been no industry shifting games realeased and reviewed in the last 90 days"),
    h4("For Full:"),
    h6("There is an obvious minimum for MetaScore that has been used by critics. If it is an average game, the MetaScore will hover around 67. All games above this line are considered to be above average games, biut there is no significant correlation that would suggest that the number of critics has anything to do with the MetaScore a game recieves."),
  mainPanel(
    plotOutput("NNextPlot")
  ),
  mainPanel( 
    h4("For Top 100 All Time:"),
    h6("Because most games do not have a lot of reviews, we figured that 2000 reviews on a game was the max we wanted to include. After looking at the relationship between the Number of Reviews and the USerScore, we see thatr the number of reviews really has little to do with the score given to a given game. There is a slight increase, but the standard error in the averages doesn't show a huge positive correlation, therefore as long as you get to a threshld of 250 reviews, you'll be an average rated game."),
    h4("For Past 90 Days:"),
    h6("In the past 90 days, not a lot of games reached the 250 review threshold, which is to be expected as most of these games are long and have stories to complete before one can review it in it's entirety. Based on the data we have, again, we can fail to say if the number of reviews has anything to do with whether or not the score for the game is higher."),
    h4("For Full:"),
    h6("With all of the games included in our plot, it is clear to see that if the game reaches a threshold of 250 reviews, it is safe to say that the game will be rated at least 7.0 by Users. This is because the biggest games that are released every year are usually by big companies and are very saught after once they are out. Given that the hype for these games is so large, I think its safe to speculate that the games with more people playing are going to recieve higher ratings because the factor of anticipation comes into the User's mind when rating the game.")),
  mainPanel(
    plotOutput("NNNextPlot")
  ), 
  mainPanel(
    h4("For Top 100 All Time:"),
    h6("We noticed that the MetaCritic website very rarely assigns more than 100 critics on one game, so we capped the User Reviews at 5000. The graph very clearly shows a positive linear correlation with the more the USer Reviews, the more the Critic Reviews."),
    h4("For Past 90 Days:"),
    h6("In the past 90 days, the trend of a positive correlation continues. With less reviews to work with, the data still shows an upward sloping correlation."),
    h4("For Full:"),
    h6("When we use the Full Data set, we see that the slope of the line is much steeper as the number of critics is below 60. Once it gets to 60, the slope tends to level out a bit, and that is because of the limit of number of critics that can review any one game. There is no limit to how many users can review a game, and so that is why there are so many more and the x-axis is weighted towards the front of the axis."))
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
      geom_jitter(mapping=aes(x=MetaScore,y=User.Score))+
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
       geom_jitter(mapping=aes(x=Critics,y=MetaScore))+
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
       geom_jitter(aes(x=Number.of.Reviews,y=User.Score))+
       geom_smooth(aes(x=Number.of.Reviews,y=User.Score)) +
       xlim(0,2000)
     
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
       geom_jitter(aes(x=Number.of.Reviews,y=Critics))+#Correlation
       geom_smooth(aes(x=Number.of.Reviews,y=Critics))+
       xlim(0,3000)
   })
   output$CrossPlatform <- renderPlot({
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
})
}


# Run the application 
shinyApp(ui = ui, server = server)

