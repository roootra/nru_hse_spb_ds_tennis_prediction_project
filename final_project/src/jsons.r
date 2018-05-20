library(RCurl)
library(XML)
library(stringr)
library(httr)
library(tidyr)
url_atp <- "http://www.atpworldtour.com/en/rankings/singles/?rankDate=2018-5-14&countryCode=all&rankRange=0-1500"
atp_chart_response <- htmlParse(file = url_atp, isURL = TRUE)

atp_rating_data <- data.frame(name = as.character(), rank = integer(), points = integer(), stringsAsFactors = FALSE)
for(i in 1:1500){
  atp_rating_data[i,"rank"] <- xpathApply(atp_chart_response, paste0('//*[@id="rankingDetailAjaxContainer"]/table/tbody/tr[', i, ']/td[1]'), xmlValue)[[1]]
  atp_rating_data[i,"name"] <- xpathApply(atp_chart_response, paste0('//*[@id="rankingDetailAjaxContainer"]/table/tbody/tr[', i, ']/td[4]/a'), xmlValue)[[1]]
  atp_rating_data[i,"points"] <- xpathApply(atp_chart_response, paste0('//*[@id="rankingDetailAjaxContainer"]/table/tbody/tr[', i, ']/td[6]/a'), xmlValue)[[1]]
}
atp_rating_data$rank <- gsub(pattern = '\\t', replacement = '', atp_rating_data$rank)
atp_rating_data$rank <- gsub(pattern = '\\n', replacement =  '', atp_rating_data$rank)
atp_rating_data$rank <- gsub(pattern = '\\r', replacement = '', atp_rating_data$rank)
atp_rating_data$points <- gsub(pattern = ',', replacement = '', atp_rating_data$points)
atp_rating_data$rank <- as.integer(atp_rating_data$rank)
atp_rating_data$points <- as.integer(atp_rating_data$points)
atp_rating_data <- rbind(c("---", 0, 0), atp_rating_data)
for(i in 1:nrow(atp_rating_data)){
  if(is.na(atp_rating_data$rank[i])){
    j <- i - 1 #чтобы лучше работало
    atp_rating_data$rank[i] <- as.numeric(atp_rating_data$rank[j]) + 1
  }
}
rm(i,j)



url_wta <- "https://www.flashscore.com/tennis/rankings/wta/"
wta_html <- getURL(url_wta)
wta_chart_response <- htmlParse(file = wta_html)

wta_rating_data <- data.frame(name = as.character(), rank = integer(), points = integer(), stringsAsFactors = FALSE)
for(i in 1:1500){ # ', i, '
  wta_rating_data[i,"rank"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[1]'), xmlValue)[[1]]
  wta_rating_data[i,"name"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[2]/a'), xmlValue)[[1]]
  wta_rating_data[i,"points"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[4]'), xmlValue)[[1]]
}
wta_rating_data$rank <- as.numeric(wta_rating_data$rank)
wta_rating_data$points <- as.numeric(wta_rating_data$points)
wta_rating_data <- separate(wta_rating_data, name, c("surname", "first_name"), " ", extra = "merge", remove = FALSE)
wta_rating_data <- separate(wta_rating_data, first_name, c("first_name", "rest"), "\\(", extra = "merge")
wta_rating_data$rest <- NULL
wta_rating_data$name_origin <- wta_rating_data$name
wta_rating_data$name <- paste0(wta_rating_data$first_name, " ", wta_rating_data$surname)
wta_rating_data <- rbind(c("---", 0, 0), wta_rating_data)


rm(atp_chart_response, i, url_atp, url_wta, wta_chart_response, wta_html)
