#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(catboost)
library(caret)
library(stats)

###Data loading
#data with player info and stats

#predicting model
avg_model <- catboost.load_model("www/atp_avg_model.catmodel")
#getting data from Pinnacle
print(paste0("Male tennis matches from Pinnacle parsed: ", nrow(tennis_data_matches_male)))
print(paste0("Female tennis matches from Pinnacle parsed: ", nrow(tennis_data_matches_female)))

###Server routine
shinyServer(function(input, output, session) {
  
  #returns proper data depending on current chosen gender
  GetCurrentPinnacleSet <- reactive({
    if(input$PlayersGender == "Male"){
      return(tennis_data_male_selectinput)
    }
    else if(input$PlayersGender == "Female"){
      return(tennis_data_female_selectinput)
    }
    else {return("---")}
  })
  
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
  
  #updates Pinnacle matched defined by gender
  observe({
    updateSelectInput(session, "ChosenPinnacleMatch", label = "Choose a match", choices = GetCurrentPinnacleSet())
  })
  
  #gets input data
  input_data <- reactive({
    input_data <- data.frame(ranking_diff = (input$`1PlayerRanking` - input$`2PlayerRanking`),
           points_diff = (input$`1PlayerPoints` - input$`2PlayerPoints`), 
           AvgW = input$`1PlayerOdds`, 
           AvgL = input$`2PlayerOdds`,
           stringsAsFactors = TRUE)
    return(input_data)
  })
  
  #makes a prediction
  output$prob <- renderText({
    input$SubmitButton #controller
    if(isolate(input$PlayersGender == "---")){print("Choose gender first")}
    else{
    isolate(if(input$Method == 1){
      match_chosen <- data.frame(text = input$ChosenPinnacleMatch)
      match_chosen <- separate(match_chosen, text, c("num", "rest"), " ", extra = "drop")
      if(input$PlayersGender == "Male"){players_chosen <- tennis_data_matches_male[match_chosen$num,]}
      else if(input$PlayersGender == "Female"){players_chosen <- tennis_data_matches_female[match_chosen$num,]}
      players_chosen$ranking_diff <- as.numeric(players_chosen$rank.x) - as.numeric(players_chosen$rank.y)
      players_chosen$points_diff <- as.numeric(players_chosen$points.x) - as.numeric(players_chosen$points.y)
      inp <- data.frame(
        ranking_diff = players_chosen$ranking_diff,
        points_diff = players_chosen$points_diff,
        AvgW = players_chosen$periods.moneyline.home,
        AvgL = players_chosen$periods.moneyline.away)
    }
    else{inp <- isolate(input_data())})
    catboost_pred_data <- as.data.frame(model.matrix(formula(~ ranking_diff + points_diff + AvgW + AvgL), data = inp))[, -1]
    catboost_pred_pool <- catboost.load_pool(data = catboost_pred_data)
    prediction <- catboost.predict(avg_model, catboost_pred_pool, prediction_type = "Probability")
    paste0("The first player is predicted to win with probability of ", (100*round(prediction, digits = 5)), "%.\n
           The second player is predicted to win with probability of ", (100*(1-round(prediction, digits = 5))), "%.")
    }
  }
  )
  
})