#####atp
###РАБОТА С ДАННЫМИ
require(dplyr)
require(readr)
require(catboost)
require(caret)


atp <- read_csv2(
  file.choose(),
  col_types = cols(
    WRank = col_double(),
    LRank = col_double(),
    WPts = col_double(),
    LPts = col_double(),
    AvgL = col_double(),
    AvgW = col_double(),
    L4 = col_double(),
    L5 = col_double(),
    MaxL = col_double(),
    MaxW = col_double(),
    SJL = col_double(),
    SJW = col_double(),
    W4 = col_double(),
    W5 = col_double()
  )
)

#указываем, что в исходных данных первый игрок победил
atp$win <- 1

#создаем выборку для создания вариации (50/50)
atp_ind <- sample(seq_len(nrow(atp)), size = nrow(atp) * 0.5)
#промежуточные оригинальные данные с рейтингом и коэф.
interm <- atp[, c(13:16, 30:45)]
#меняем W на L (и наоборот), указываем, что первый игрок не победил
for (i in atp_ind) {
  atp[i, 14] <- interm[i, 1]
  atp[i, 13] <- interm[i, 2]
  atp[i, 16] <- interm[i, 3]
  atp[i, 15] <- interm[i, 4]
  atp[i, 31] <- interm[i, 5]
  atp[i, 30] <- interm[i, 6]
  atp[i, 33] <- interm[i, 7]
  atp[i, 32] <- interm[i, 8]
  atp[i, 35] <- interm[i, 9]
  atp[i, 34] <- interm[i, 10]
  atp[i, 37] <- interm[i, 11]
  atp[i, 36] <- interm[i, 12]
  atp[i, 39] <- interm[i, 13]
  atp[i, 38] <- interm[i, 14]
  atp[i, 41] <- interm[i, 15]
  atp[i, 40] <- interm[i, 16]
  atp[i, 43] <- interm[i, 17]
  atp[i, 42] <- interm[i, 18]
  atp[i, 45] <- interm[i, 19]
  atp[i, 44] <- interm[i, 20]
  atp$win[i] <- 0
}
rm(interm, i)

#создаем разницы между 1 и 2 игроком
atp$ranking_diff <- atp$WRank - atp$LRank
atp$points_diff <- atp$WPts - atp$LPts

#явно указываем факторы
atp$win <- as.factor(atp$win)
atp$Surface <- as.factor(atp$Surface)
atp$Court <- as.factor(atp$Court)

#данные для модели
atp_data <-
  select(atp,
         #Surface,
         #Court,
         ranking_diff,
         points_diff,
         B365W,
         B365L,
         AvgW,
         AvgL,
         win)
atp_data_avg <- select(atp_data, -B365W, -B365L)#-points_diff
atp_data_avg <- na.omit(atp_data_avg)

#FEATURE ENGINEERING
#atp_data_avg$odds_diff <- atp_data_avg$AvgW - atp_data_avg$AvgL
#atp_data_avg$vig <- 1/atp_data_avg$AvgW + 1/atp_data_avg$AvgL #intermediate
#atp_data_avg$margin <- atp_data_avg$vig - 1
#Margin Weights Proportional to the Odds
#atp_data_avg$mpto_W <- 2*atp_data_avg$AvgW / (2 - atp_data_avg$margin * atp_data_avg$AvgW)
#atp_data_avg$mpto_L <- 2*atp_data_avg$AvgL / (2 - atp_data_avg$margin * atp_data_avg$AvgL)
#Odds Ratio
#atp_data_avg$or <- atp_data_avg$AvgW/atp_data_avg$AvgL #intermediate
#atp_data_avg$c <- 1/atp_data_avg$AvgW * 1/atp_data_avg$AvgL #intermediate
#atp_data_avg$a <- (1/atp_data_avg$AvgW + 1/atp_data_avg$AvgL) - (1/atp_data_avg$AvgW * 1/atp_data_avg$AvgL) - 1 #intermediate
#atp_data_avg$S <- -((-4*atp_data_avg$a*atp_data_avg$c)^0.5)/(2*atp_data_avg$a) #intermediate
#atp_data_avg$or_W <- 1/((1/atp_data_avg$AvgW) / (atp_data_avg$S + 1/atp_data_avg$AvgW - (atp_data_avg$S * 1/atp_data_avg$AvgW)))
#atp_data_avg$or_L <- 1/((1/atp_data_avg$AvgL) / (atp_data_avg$S + 1/atp_data_avg$AvgL - (atp_data_avg$S * 1/atp_data_avg$AvgL)))
#Logarithmic Function
#opt_func <- function(x){
#  log(1/1.20, base = x) - 1/2
#}
#library(nleqslv)
#nleqslv(0.9, opt_func)
#Remove unnecessary vars
#atp_data_avg$vig <- NULL
#atp_data_avg$or <- NULL
#atp_data_avg$c <- NULL
#atp_data_avg$a <- NULL
#atp_data_avg$S <- NULL
#atp_data_avg$odds_diff <- NULL

#ЛОГИТ
#ранги не значимы, поле и помещение не значимо
library(car)
test_logit <-
  glm(as.factor(win) ~ ., data = atp_data_avg, family = binomial(logit))
summary(test_logit)
vif(test_logit)

### ТЮНИНГ

library(doMC)
registerDoMC(cores=8)

#optimal:
#depth = 13 | 7
#learning_rate = 0.05 | 0.03
#iterations = 100
#l2_leaf_reg = 2.5 | 4
#rsm = 0.3
#border_count = 80 | 64

grid <- expand.grid(
  iterations = 100,
  border_count = c(32, 80, 100),
  depth = c(2, 3, 5, 7, 9),
  learning_rate = c(0.03, 0.3),
  l2_leaf_reg = c(0, 2, 4),
  rsm = c(0.3, 0.5)
)

fit_control <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  allowParallel = TRUE
)

atp_data_avg_x <- select(atp_data_avg, -win)
attr(atp_data_avg_x, "na.action") <- NULL
atp_data_avg_y <- atp_data_avg[, c("win")]
atp_data_avg_y <- factor(atp_data_avg_y$win)

catboost_atp_model <- train(
  x = atp_data_avg_x,
  y = atp_data_avg_y,
  method = catboost.caret,
  trControl = fit_control,
  tuneGrid = grid,
  preProc = NULL,
  logging_level = "Verbose",
  eval_metric = 'Accuracy'
)#debug,info,verbose
varImp(catboost_atp_model)

###МОДЕЛЬ
#разделяем выборку на тестовую и обучающую
atp_ind_test = sample(seq_len(nrow(atp_data_avg)), size = nrow(atp_data_avg) *
                        0.2)
atp_data_avg.test = atp_data_avg[atp_ind_test, ]
atp_data_avg.train = atp_data_avg[-atp_ind_test, ]
#убираем аттрибуты
attr(atp_data_avg.test, "na.action") <- NULL
attr(atp_data_avg.train, "na.action") <- NULL
atp_data_avg.test <- as.data.frame(atp_data_avg.test)
atp_data_avg.train <- as.data.frame(atp_data_avg.train)

atp_data_avg.test$ranking_diff <-
  as.double(atp_data_avg.test$ranking_diff)
atp_data_avg.test$AvgW <- as.double(atp_data_avg.test$AvgW)
atp_data_avg.test$AvgL <- as.double(atp_data_avg.test$AvgL)
atp_data_avg.train$ranking_diff <-
  as.double(atp_data_avg.train$ranking_diff)
atp_data_avg.train$AvgW <- as.double(atp_data_avg.train$AvgW)
atp_data_avg.train$AvgL <- as.double(atp_data_avg.train$AvgL)

atp_data_avg.test.model <-
  as.data.frame(model.matrix( ~ ., atp_data_avg.test))[, -1]
atp_data_avg.train.model <-
  as.data.frame(model.matrix( ~ ., atp_data_avg.train))[, -1]


#создаем пул (данные для catboost'a)
atp_pool.train <-
  catboost.load_pool(data = atp_data_avg.train.model[, -5], label = atp_data_avg.train.model[, "win1"]) #-8
atp_pool.test <-
  catboost.load_pool(data = atp_data_avg.test.model[, -5], label = atp_data_avg.test.model[, "win1"]) #-8

#catboost.save_pool(atp_pool.train, pool_path = "atp_avg_pool_train.pool", cd_path = "atp_avg_pool_train.cd")
#catboost.save_pool(atp_pool.test, pool_path = "atp_avg_pool_test.pool", cd_path = "atp_avg_pool_test.cd")

#native func
fit_params <- list(
  iterations = 1000,
  thread_count = 8,
  loss_function = 'Logloss:Border=0.5',
  eval_metric = 'Accuracy',
  rsm = 0.3,
  logging_level = "Verbose",
  border_count = 32,
  depth = 3,
  learning_rate = 0.03,
  l2_leaf_reg = 4,
  train_dir = 'train_dir'
)

catboost_atp_avg_model <-
  catboost.train(atp_pool.train, atp_pool.test, params = fit_params)
catboost.save_model(catboost_atp_avg_model, "atp_avg_model.catmodel")
catboost_atp_avg_model

catboost.get_feature_importance(catboost_atp_avg_model)
#catboost.get_model_params(catboost_atp_avg_model)

catboost_pred <- catboost.predict(catboost_atp_avg_model, atp_pool.test, prediction_type = "Probability")

###Auto ML
library(h2o)
h2o.init(nthreads = 8)
y <- "win"
x <- setdiff(names(atp_data_avg.train), y)
h2o_train_atp <- as.h2o(atp_data_avg.train)
h2o_test_atp <- as.h2o(atp_data_avg.test)
aml <-
  h2o.automl(
    x = x,
    y = y,
    training_frame = h2o_train_atp,
    validation_frame = h2o_test_atp,
    stopping_metric = "AUTO", #accuracy или auc?
    max_runtime_secs = 10000
  )
aml




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
wta_data_avg <- select(wta_data, -B365W, -B365L)
wta_data_avg <- na.omit(wta_data_avg)

#FEATURE ENGINEERING
wta_data_avg$odds_diff <- wta_data_avg$AvgW - wta_data_avg$AvgL
wta_data_avg$vig <- 1/wta_data_avg$AvgW + 1/wta_data_avg$AvgL #intermediate
wta_data_avg$margin <- wta_data_avg$vig - 1
#Margin Weights Proportional to the Odds
wta_data_avg$mpto_W <- 2*wta_data_avg$AvgW / (2 - wta_data_avg$margin * wta_data_avg$AvgW)
wta_data_avg$mpto_L <- 2*wta_data_avg$AvgL / (2 - wta_data_avg$margin * wta_data_avg$AvgL)
#Odds Ratio
wta_data_avg$or <- wta_data_avg$AvgW/wta_data_avg$AvgL #intermediate
wta_data_avg$c <- 1/wta_data_avg$AvgW * 1/wta_data_avg$AvgL #intermediate
wta_data_avg$a <- (1/wta_data_avg$AvgW + 1/wta_data_avg$AvgL) - (1/wta_data_avg$AvgW * 1/wta_data_avg$AvgL) - 1 #intermediate
wta_data_avg$S <- -((-4*wta_data_avg$a*wta_data_avg$c)^0.5)/(2*wta_data_avg$a) #intermediate
wta_data_avg$or_W <- 1/((1/wta_data_avg$AvgW) / (wta_data_avg$S + 1/wta_data_avg$AvgW - (wta_data_avg$S * 1/wta_data_avg$AvgW)))
wta_data_avg$or_L <- 1/((1/wta_data_avg$AvgL) / (wta_data_avg$S + 1/wta_data_avg$AvgL - (wta_data_avg$S * 1/wta_data_avg$AvgL)))
#Logarithmic Function
#opt_func <- function(x){
#  log(1/1.20, base = x) - 1/2
#}
#library(nleqslv)
#nleqslv(0.9, opt_func)
#Remove unnecessary vars
wta_data_avg$vig <- NULL
wta_data_avg$or <- NULL
wta_data_avg$c <- NULL
wta_data_avg$a <- NULL
wta_data_avg$S <- NULL

#логит
#Трава значима!
library(car)
test_logit <- glm(as.factor(win) ~ ., data = wta_data_avg, family=binomial(logit))
summary(test_logit)
vif(test_logit)

library(doMC)
registerDoMC(cores=8)

grid <- expand.grid(iterations = 100,
                    border_count = c(64,80,96),
                    depth = c(2,3,5,7,9,12),
                    learning_rate = c(0.01, 0.03, 0.1, 0.3),
                    l2_leaf_reg = c(0,1,2,3),
                    rsm = c(0.3, 0.5, 0.7)
)

fit_control <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  allowParallel = TRUE
)

wta_data_avg_x <- select(wta_data_avg, -win)
attr(wta_data_avg_x, "na.action") <- NULL
wta_data_avg_y <- wta_data_avg[,c("win")]
wta_data_avg_y <- factor(wta_data_avg_y$win)

catboost_wta_model <- train(x = wta_data_avg_x, y = wta_data_avg_y,
                            method=catboost.caret, trControl = fit_control, 
                            tuneGrid = grid, preProc = NULL, 
                            logging_level = "Verbose")#debug,info,verbose

###МОДЕЛЬ
#разделяем выборку на тестовую и обучающую
wta_ind_test = sample(seq_len(nrow(wta_data_avg)), size = nrow(wta_data_avg)*0.2)
wta_data_avg.test = wta_data_avg[wta_ind_test,]
wta_data_avg.train = wta_data_avg[-wta_ind_test,]
#убираем аттрибуты
attr(wta_data_avg.test, "na.action") <- NULL
attr(wta_data_avg.train, "na.action") <- NULL
wta_data_avg.test <- as.data.frame(wta_data_avg.test)
wta_data_avg.train <- as.data.frame(wta_data_avg.train)

wta_data_avg.test$ranking_diff <- as.double(wta_data_avg.test$ranking_diff)
wta_data_avg.test$AvgW <- as.double(wta_data_avg.test$AvgW)
wta_data_avg.test$AvgL <- as.double(wta_data_avg.test$AvgL)
wta_data_avg.train$ranking_diff <- as.double(wta_data_avg.train$ranking_diff)
wta_data_avg.train$AvgW <- as.double(wta_data_avg.train$AvgW)
wta_data_avg.train$AvgL <- as.double(wta_data_avg.train$AvgL)

wta_data_avg.test$Surface <- factor(wta_data_avg.test$Surface)
wta_data_avg.train$Surface <- factor(wta_data_avg.train$Surface)

wta_data_avg.test.model <- as.data.frame(model.matrix(~., wta_data_avg.test))[,-1]
wta_data_avg.train.model <- as.data.frame(model.matrix(~., wta_data_avg.train))[,-1]


#создаем пул (данные для catboost'a)
wta_pool.train <- catboost.load_pool(data = wta_data_avg.train.model[,-8], label = wta_data_avg.train.model[,"win1"])
wta_pool.test <- catboost.load_pool(data = wta_data_avg.test.model[,-8], label = wta_data_avg.test.model[,"win1"])

catboost.save_pool(wta_pool.train, pool_path = "wta_avg_pool_train.pool", cd_path = "wta_avg_pool_train.cd")
catboost.save_pool(wta_pool.test, pool_path = "wta_avg_pool_test.pool", cd_path = "wta_avg_pool_test.cd")

#native func
fit_params <- list(iterations = 1000,
                   thread_count = 8,
                   loss_function = 'Logloss:Border=0.5',
                   eval_metric = 'AUC',
                   rsm = 0.3,
                   logging_level = "Verbose",
                   border_count = 96,
                   depth = 2,
                   learning_rate = 0.1,
                   l2_leaf_reg = 3,
                   train_dir = 'train_dir')

catboost_wta_avg_model <- catboost.train(wta_pool.train, wta_pool.test, params = fit_params)
catboost.save_model(catboost_wta_avg_model, "wta_avg_model.catmodel")
catboost_wta_avg_model

catboost.get_feature_importance(catboost_wta_avg_model)
catboost.get_model_params(catboost_wta_avg_model)

###Auto ML
library(h2o)
h2o.init(nthreads = 8, strict_version_check = FALSE)
y <- "win"
x <- setdiff(names(wta_data_avg.train), y)
h2o_train_wta <- as.h2o(wta_data_avg.train)
h2o_test_wta <- as.h2o(wta_data_avg.test)
aml <- h2o.automl(x=x, y=y, training_frame = h2o_train_wta, validation_frame = h2o_test_wta)
aml
