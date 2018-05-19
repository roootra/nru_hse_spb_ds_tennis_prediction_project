#####ATP
###РАБОТА С ДАННЫМИ
library(dplyr)
library(readr)

atp <- read_csv2(file.choose(), 
                col_types = cols(WRank = col_double(), LRank = col_double(),
                                 WPts = col_double(), LPts = col_double(),
                                 AvgL = col_double(), 
                                 AvgW = col_double(), L4 = col_double(), 
                                 L5 = col_double(), MaxL = col_double(), 
                                 MaxW = col_double(), SJL = col_double(), 
                                 SJW = col_double(), W4 = col_double(), 
                                 W5 = col_double()))

#указываем, что в исходных данных первый игрок победил
atp$win <- 1

#создаем выборку для создания вариации (50/50)
atp_ind <- sample(seq_len(nrow(atp)), size = nrow(atp)*0.5)
#промежуточные оригинальные данные с рейтингом и коэф.
interm <- atp[,c(13:16,30:45)]
#меняем W на L (и наоборот), указываем, что первый игрок не победил
for(i in atp_ind){
  atp[i,14] <- interm[i,1]
  atp[i,13] <- interm[i,2]
  atp[i,16] <- interm[i,3]
  atp[i,15] <- interm[i,4]
  atp[i,31] <- interm[i,5]
  atp[i,30] <- interm[i,6]
  atp[i,33] <- interm[i,7]
  atp[i,32] <- interm[i,8]
  atp[i,35] <- interm[i,9]
  atp[i,34] <- interm[i,10]
  atp[i,37] <- interm[i,11]
  atp[i,36] <- interm[i,12]
  atp[i,39] <- interm[i,13]
  atp[i,38] <- interm[i,14]
  atp[i,41] <- interm[i,15]
  atp[i,40] <- interm[i,16]
  atp[i,43] <- interm[i,17]
  atp[i,42] <- interm[i,18]
  atp[i,45] <- interm[i,19]
  atp[i,44] <- interm[i,20]
  atp$win[i] <- 0
}
rm(interm,i)

#создаем разницы между 1 и 2 игроком
atp$ranking_diff <- atp$WRank - atp$LRank
atp$points_diff <- atp$WPts - atp$LPts

#явно указываем факторы
atp$win <- as.factor(atp$win)
atp$Surface <- as.factor(atp$Surface)
atp$Court <- as.factor(atp$Court)

#данные для модели
atp_data <- select(atp, Surface, Court, ranking_diff, points_diff, B365W, B365L, AvgW, AvgL, win)
atp_data_avg <- select(atp_data, -B365W, -B365L, -points_diff)
atp_data_avg <- na.omit(atp_data_avg)

#ЛОГИТ
#ранги не значимы, поле и помещение не значимо
library(car)
test_logit <- glm(as.factor(win) ~ ., data = atp_data_avg, family=binomial(logit))
summary(test_logit)
vif(test_logit)

#разделяем выборку на тестовую и обучающую
atp_ind_test = sample(seq_len(nrow(atp_data_avg)), size = nrow(atp_data_avg)*0.2)
atp_data_avg.test = atp_data_avg[atp_ind_test,]
atp_data_avg.train = atp_data_avg[-atp_ind_test,]

##################################################################################################################
#####WTA
###РАБОТА С ДАННЫМИ

wta <- read_csv2(file.choose(), 
                col_types = cols(WRank = col_double(), LRank = col_double(),
                                 WPts = col_double(), LPts = col_double(),
                                 AvgL = col_double(), 
                                 AvgW = col_double(), MaxL = col_double(), 
                                 MaxW = col_double(), SJL = col_double(), 
                                 SJW = col_double()))


wta$win <- 1
wta_ind <- sample(seq_len(nrow(wta)), size = nrow(wta)*0.5)

interm <- wta[,c(13:16,26:41)]

for(i in wta_ind){
  wta[i,14] <- interm[i,1]
  wta[i,13] <- interm[i,2]
  wta[i,16] <- interm[i,3]
  wta[i,15] <- interm[i,4]
  wta[i,27] <- interm[i,5]
  wta[i,26] <- interm[i,6]
  wta[i,29] <- interm[i,7]
  wta[i,28] <- interm[i,8]
  wta[i,31] <- interm[i,9]
  wta[i,30] <- interm[i,10]
  wta[i,33] <- interm[i,11]
  wta[i,32] <- interm[i,12]
  wta[i,35] <- interm[i,13]
  wta[i,34] <- interm[i,14]
  wta[i,37] <- interm[i,15]
  wta[i,36] <- interm[i,16]
  wta[i,39] <- interm[i,17]
  wta[i,38] <- interm[i,18]
  wta[i,41] <- interm[i,19]
  wta[i,40] <- interm[i,20]
  wta$win[i] <- 0
}
rm(interm,i)

#создаем разницы между 1 и 2 игроком
wta$ranking_diff <- wta$WRank - wta$LRank
wta$points_diff <- wta$WPts - wta$LPts

#явно указываем факторы
wta$win <- as.factor(wta$win)
wta$Surface <- as.factor(wta$Surface)
wta$Court <- as.factor(wta$Court)

#данные для модели
wta_data <- select(wta, Surface, Court, ranking_diff, points_diff, B365W, B365L, AvgW, AvgL, win)
wta_data_avg <- select(wta_data, -B365W, -B365L, -points_diff)
wta_data_avg <- na.omit(wta_data_avg)

#логит
#Трава значима!
library(car)
test_logit <- glm(as.factor(win) ~ ., data = wta_data_avg, family=binomial(logit))
summary(test_logit)
vif(test_logit)
