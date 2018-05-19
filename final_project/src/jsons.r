library(RCurl)
library(XML)
library(stringr)
library(httr)
url_atp <- "http://www.atpworldtour.com/en/rankings/singles/?rankDate=2018-4-30&countryCode=all&rankRange=0-300"
atp_chart_response <- htmlParse(file = url_atp, isURL = TRUE)

atp_rating_data <- data.frame(name = as.character(), rank = integer(), points = integer(), stringsAsFactors = FALSE)
for(i in 1:300){
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



url_wta <- "https://www.flashscore.com/tennis/rankings/wta/"
wta_html <- getURL(url_wta)
wta_chart_response <- htmlParse(file = wta_html)

wta_rating_data <- data.frame(name = as.character(), rank = integer(), points = integer(), stringsAsFactors = FALSE)
for(i in seq(1, 300, by = 1)){ # ', i, '
  wta_rating_data[i,"rank"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[1]'), xmlValue)[[1]]
  wta_rating_data[i,"name"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[2]/a'), xmlValue)[[1]]
  wta_rating_data[i,"points"] <- xpathApply(wta_chart_response, paste0('//*[@id="ranking-table-results_35853"]/tbody/tr[', i, ']/td[4]'), xmlValue)[[1]]
}
wta_rating_data$rank <- as.numeric(wta_rating_data$rank)
wta_rating_data$points <- as.numeric(wta_rating_data$points)
wta_rating_data <- rbind(c("---", 0, 0), wta_rating_data)

rm(atp_chart_response, i, url_atp, url_wta, wta_chart_response, wta_html)
