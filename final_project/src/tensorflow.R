library(tensorflow)
library(keras)
library(dplyr)

str(atp_data_avg.train)
str(atp_data_avg.test)
train_y <- atp_data_avg.train$win
train_x <- select(atp_data_avg.train, -win, -Surface, -Court)
test_y <- atp_data_avg.test$win
test_x <- select(atp_data_avg.test, -win,  -Surface, -Court)

train_x <- sapply(train_x, scale, center = FALSE)
test_x <- sapply(test_x, scale, center = FALSE)
train_y <- ifelse(as.numeric(train_y) == 1, 0, 1)
test_y <- ifelse(as.numeric(test_y) == 1, 0, 1)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = 10) %>% #64
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 256, activation = 'relu') %>% #64
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 256, activation = 'relu') %>% #32
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = loss_binary_crossentropy,
  optimizer = optimizer_rmsprop(),
  metrics = 'accuracy'
)
model %>% fit(train_x, train_y, epochs = 100, batch_size = 128)
model %>% evaluate(test_x, test_y, batch_size = 16)

ntm <- keras_model_sequential()
ntm %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = 10) %>% 
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 48, activation = 'relu') %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 48, activation = 'relu') %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate=0.4) %>%
  layer_lstm(units = 64, activation = 'relu', return_sequences = TRUE,) %>%
  layer_dropout(units = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')

ntm %>% compile(
  loss = loss_binary_crossentropy,
  optimizer = optimizer_rmsprop(),
  metrics = 'accuracy'
)
ntm %>% fit(train_x, train_y, epochs = 100, batch_size = 128)
ntm %>% evaluate(test_x, test_y, batch_size = 16)