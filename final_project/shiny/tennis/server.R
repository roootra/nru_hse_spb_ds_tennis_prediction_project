#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(catboost)
library(caret)
library(stats)

###Data loading
#data with player info and stats
load("www/parsed_data.RData")
#predicting model
avg_model <- catboost.load_model("www/avg_model.catmodel")

###Server routine
shinyServer(function(input, output, session) {
  
  #returns proper data depending on current chosen gender
  player_list <- reactive({
    if(input$PlayersGender == "Male") {return(atp_rating_data)}
    else if(input$PlayersGender == "Female") {return(wta_rating_data)}
    else return(NULL)
  })
  
  #updates players' names depending on gender
  observe({
  updateSelectInput(session, "Player1Name", label = "Player #1 Name", choices = player_list()$name)
  updateSelectInput(session, "Player2Name", label = "Player #2 Name", choices = player_list()$name)
  })
  
  #updates ranks and points depending on players' names
  observe({
    updateNumericInput(session, "1PlayerRanking", label = "Player #1 ATP/WTA Ranking", value = player_list()[player_list()$name == input$Player1Name,]$rank, min = 0, max = 10000)
    updateNumericInput(session, "2PlayerRanking", label = "Player #2 ATP/WTA Ranking", value = player_list()[player_list()$name == input$Player2Name,]$rank, min = 0, max = 10000)
    updateNumericInput(session, "1PlayerPoints", label = "Player #1 ATP/WTA Points", value = player_list()[player_list()$name == input$Player1Name,]$points, min = 0, max = 10000)
    updateNumericInput(session, "2PlayerPoints", label = "Player #2 ATP/WTA Points", value = player_list()[player_list()$name == input$Player2Name,]$points, min = 0, max = 10000)
  })
  
  #gets input data
  input_data <- reactive({
    input_data <- data.frame(Surface = input$Surface, Court = input$Court, ranking_diff = (input$`1PlayerRanking` - input$`2PlayerRanking`),
           points_diff = (input$`1PlayerPoints` - input$`2PlayerPoints`), AvgW = input$`1PlayerOdds`, AvgL = input$`2PlayerOdds`, 
           stringsAsFactors = TRUE)
    levels(input_data$Surface) <- c("Clay",  "Grass", "Hard")
    levels(input_data$Court) <- c("Indoor",  "Outdoor")
    return(input_data)
  })
  
  #makes a prediction
  output$prob <- renderText({
    input$SubmitButton #controller
    inp <- isolate(input_data())
    catboost_pred_data <- as.data.frame(model.matrix(formula(~ Surface + Court + ranking_diff + points_diff + AvgW + AvgL), data = inp))[, -1]
    catboost_pred_pool <- catboost.load_pool(data = catboost_pred_data)
    prediction <- catboost.predict(avg_model, catboost_pred_pool, prediction_type = "Probability")
    paste0("The first player is predicted to win with a probability of ", (100*round(prediction, digits = 5)), "%.")
  })
  
})