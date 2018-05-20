#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#CSS (https://html5book.ru/krasivoe-oformlenie-citat-na-sayte/)

library(shiny)
#Tennis players' quotes
quotes <- readLines("www/quotes.txt")
quotes_df <- as.data.frame(matrix(quotes, ncol=2, byrow=TRUE))
colnames(quotes_df) <- c("Quote", "Author")
quotes_df$Author <- gsub("―", "", quotes_df$Author)
number <- runif(1, 1, 80)
number <- round(number, digits = 0)


#UI routine
shinyUI(fixedPage(theme = "style.css",
  
  #Title
  HTML("<center><h2>Tennis match winner prediction service</h2></center>"),
  br(),
  HTML(paste0("<center><blockquote><p><span>",quotes_df$Quote[number],
              "</span></p><footer>— <cite>",quotes_df$Author[number],
              "</cite></footer></blockquote></center>")),
  br(),
  hr(),
  #GUI
  fixedRow(
    column(selectInput(inputId="PlayersGender", label = "Players Gender", choices = c("---","Male", "Female"), selected = 1), width = 12, align = 'center')
      ),
  fixedRow(
    column(selectInput(inputId="Player1Name", label = "Player #1 Name", choices = player_list), width = 6, align = 'right'),
    column(selectInput(inputId="Player2Name", label = "Player #2 Name", choices = player_list), width = 6, align = 'left')
  ),
  fixedRow(
    column(numericInput(inputId="1PlayerRanking", label = "Player #1 ATP/WTA Ranking", value = 1, min = 0, max = 5000), width = 6, align = 'right'),
    column(numericInput(inputId="2PlayerRanking", label = "Player #2 ATP/WTA Ranking", value = 1, min = 0, max = 5000), width = 6, align = 'left')
      ),
  fixedRow(
    column(numericInput(inputId="1PlayerPoints", label = "Player #1 ATP/WTA Points", value = 1, min = 0, max = 10000), width = 6, align = 'right'),
    column(numericInput(inputId="2PlayerPoints", label = "Player #2 ATP/WTA Points", value = 1, min = 0, max = 10000), width = 6, align = 'left')
  ),
  fixedRow(
    column(numericInput(inputId="1PlayerOdds", label = "Player #1 ATP/WTA Average Odds", value = 1.00, min = 0.01, max = 100.00, step = 0.01), width = 6, align = 'right'),
    column(numericInput(inputId="2PlayerOdds", label = "Player #2 ATP/WTA Average Odds", value = 1.00, min = 0.01, max = 100.00, step = 0.01), width = 6, align = 'left')
  ),
  
  fixedRow(
    column(actionButton(inputId = "SubmitButton", "Make a prediction!"), width = 12, align = 'center')
    ),
  
  br(),
  
  fixedRow(
    column(textOutput("prob"), width = 12, align = 'center')
  ),
  
  br(),
  br(),
  br(),
  br(),
  br()
))
