#shared data
player_list <- NULL

#getPinnacleOdds
{
  library(pinnacle.API)
  library(dplyr)
  AcceptTermsAndConditions(accepted = TRUE) # :)
  SetCredentials("AS1047100", "9!J71lju1oo")
  
  #получаем данные по теннису
  # sports_data <- GetSports(force = FALSE)
  # tennis_id <- with(sports_data, id[name == 'Tennis'])
  # tennis_data <- showOddsDF(tennis_id, oddsformat = 'Decimal', force = FALSE)
  # tennis_data_selected <- select(tennis_data, 
  #                                periods.moneyline.home, periods.moneyline.away, league.name,
  #                                league.events.starts, league.events.home, league.events.away,
  #                                league.events.status, leagues.container, events.periods.number)
  # 
  # tennis_data_selected <- tennis_data_selected[-grep("Doubles", tennis_data_selected$league.name),]
  # tennis_data_selected <- filter(tennis_data_selected, events.periods.number == 0)
  # tennis_data_selected <- tennis_data_selected[-grep("([A-Za-z]+) ([A-Za-z]+) To|Game|\\(", 
  #                                                    tennis_data_selected$league.events.home),]
  #dummy для ассоциаций
  # tennis_data_selected$isATP <- 0
  # tennis_data_selected$isATP[grep("ATP", tennis_data_selected$league.name)] <- 1
  # tennis_data_selected$isWTA <- 0
  # tennis_data_selected$isWTA[grep("WTA", tennis_data_selected$league.name)] <- 1
  
  #датасет с заполненными мужчинами
  # tennis_data_matches_male <- inner_join(tennis_data_selected, atp_rating_data, 
  #                                        by = c("league.events.home" = "name"))
  # tennis_data_matches_male$rank_player_1 <- tennis_data_matches_male$rank
  # tennis_data_matches_male$points_player_1 <- tennis_data_matches_male$points
  # tennis_data_matches_male <- inner_join(tennis_data_matches_male, atp_rating_data, 
  #                                        by = c("league.events.away" = "name"))
  # tennis_data_matches_male$rank_player_2 <- tennis_data_matches_male$rank
  # tennis_data_matches_male$points_player_2 <- tennis_data_matches_male$points
  # tennis_data_matches_male$rank <- NULL
  # tennis_data_matches_male$points <- NULL
  # tennis_data_matches_male$id <- seq(1, nrow(tennis_data_matches_male))
  
  #датасет с заполненными женщинами
  # tennis_data_matches_female <- inner_join(tennis_data_selected, wta_rating_data, 
  #                                          by = c("league.events.home" = "name"))
  # tennis_data_matches_female$rank_player_1 <- tennis_data_matches_female$rank
  # tennis_data_matches_female$points_player_1 <- tennis_data_matches_female$points
  # tennis_data_matches_female <- inner_join(tennis_data_matches_female, wta_rating_data, 
  #                                          by = c("league.events.away" = "name"))
  # tennis_data_matches_female$rank_player_2 <- tennis_data_matches_female$rank
  # tennis_data_matches_female$points_player_2 <- tennis_data_matches_female$points
  # tennis_data_matches_female$rank <- NULL
  # tennis_data_matches_female$points <- NULL
  # tennis_data_matches_female$id <- seq(1, nrow(tennis_data_matches_female))
  
  # данные для вывода в selectInput
  tennis_data_male_selectinput <- paste(
        tennis_data_matches_male$id,
        "—",
        tennis_data_matches_male$league.events.home,
        "vs.",
        tennis_data_matches_male$league.events.away,
        "|",
        tennis_data_matches_male$league.name,
        "Odds:",
        tennis_data_matches_male$periods.moneyline.home,
        "–",
        tennis_data_matches_male$periods.moneyline.away
  )
  
  tennis_data_female_selectinput <- paste(
        tennis_data_matches_female$id,
        "—",
        tennis_data_matches_female$league.events.home,
        "vs.",
        tennis_data_matches_female$league.events.away,
        "|",
        tennis_data_matches_female$league.name,
        "Odds:",
        tennis_data_matches_female$periods.moneyline.home,
        "–",
        tennis_data_matches_female$periods.moneyline.away
  )
}