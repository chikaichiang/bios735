library(caret)
library(tidyverse)
library(NeuralNetTools)

# ---- Setup paths ----
data_dir <- file.path(getwd(), "Data")  # Assuming the files are in a "Data" folder under current directory

train_file <- file.path(data_dir, "wine_train_scaled.csv")
test_file  <- file.path(data_dir, "wine_test_scaled.csv")

# ---- Check for file existence ----
if (!file.exists(train_file)) stop(paste("Training file not found:", train_file))
if (!file.exists(test_file)) stop(paste("Test file not found:", test_file))

# ---- Load data ----
train_data <- read.csv(train_file)
test_data  <- read.csv(test_file)

# numeric encoding: white = 0, red = 1
train_data$color_num <- ifelse(train_data$color == "red", 1, 0)
test_data$color_num  <- ifelse(test_data$color == "red", 1, 0)

# predictors only
preds <- setdiff(names(train_data), 
                 c("quality","quality_7","color","color_num"))

# 2. trainControl for regression CV
ctrl <- trainControl(
  method = "cv",
  number = 5
)

# 3. tuning grid for two hidden layers (3,2)
grid_nn <- expand.grid(
  layer1 = c(5,4,3),
  layer2 = c(3,2,1),
  layer3 = 0
)

# 4. Train the model as regression
set.seed(12345)
nn_reg <- train(
  x          = train_data[, preds],
  y          = train_data$color_num,
  method     = "neuralnet",
  trControl  = ctrl,
  tuneGrid   = grid_nn,
  linear.output = FALSE,   # logistic output
  act.fct       = "logistic",
  stepmax       = 1e6
)

print(nn_reg)
plot(nn_reg$finalModel)

# get continuous predictions and threshold
pred_cont <- predict(nn_reg, test_data[, preds])
pred_class <- factor(ifelse(pred_cont > 0.5, "red", "white"),
                     levels = c("red","white"))

confusionMatrix(pred_class, 
                factor(test_data$color, levels = c("red","white")),
                positive = "red")

old=olden(nn_reg$finalModel)$data
