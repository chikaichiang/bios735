library(caret)
library(tidyverse)

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

# predictors only
preds <- setdiff(names(train_data), 
                 c("quality","quality_7","color","color_num"))

train_data$quality_7 <- ifelse(train_data$quality_7 == 1, 1, 0)
test_data$quality_7  <- ifelse(test_data$quality_7  == 1, 1, 0)
ctrl   <- trainControl(method="cv", number=5)
grid_nn <- expand.grid(
  layer1 = c(5,4,3),
  layer2 = c(3,2,1),
  layer3 = 0
)

nn_reg <- train(
  x             = train_data[, preds],
  y             = train_data$quality_7,
  method        = "neuralnet",
  trControl     = ctrl,
  tuneGrid      = grid_nn,
  linear.output = FALSE,
  act.fct       = "logistic"
)
pred_cont  <- predict(nn_reg, test_data[, preds])
pred_factor<- factor(ifelse(pred_cont > 0.5, "1", "0"),
                     levels = c("0","1"))
ref_factor <- factor(test_data$quality_7, levels = c(0,1))

confusionMatrix(pred_factor, ref_factor, positive = "1")
old=olden(nn_reg$finalModel)$data
