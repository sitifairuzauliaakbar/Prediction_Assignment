# Prediction_Assignment

## Siti Fairuz Aulia Akbar

### Data Sources:
- [Training Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)
- [Testing Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

## Load Required Libraries
    library(randomForest)
    library(caret)

## Load and Preprocess Data
    train_data <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
    test_data <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

## Remove irrelevant columns
    irrelevant_cols <- c("X", "user_name", "cvtd_timestamp", "raw_timestamp_part_1", "raw_timestamp_part_2", "new_window", "num_window")
    train_data <- train_data[, !(names(train_data) %in% irrelevant_cols)]
    test_data <- test_data[, !(names(test_data) %in% irrelevant_cols)]

## Convert non-numeric columns to numeric
    train_data <- data.frame(lapply(train_data, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x))
    test_data <- data.frame(lapply(test_data, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x))

## Remove columns with too many NAs
    na_threshold <- 0.7
    valid_cols <- colSums(is.na(train_data)) / nrow(train_data) < na_threshold
    train_data <- train_data[, valid_cols]
    test_data <- test_data[, names(train_data)[-ncol(train_data)]]

## Convert target variable to factor
    train_data$classe <- as.factor(train_data$classe)

## Train-Test Split
    set.seed(123)
    trainIndex <- createDataPartition(train_data$classe, p = 0.8, list = FALSE)
    train_set <- train_data[trainIndex, ]
    valid_set <- train_data[-trainIndex, ]

## Train Random Forest Model
    set.seed(123)
    rf_model <- randomForest(classe ~ ., data = train_set, ntree = 500, mtry = sqrt(ncol(train_set) - 1), importance = TRUE)

## Evaluate Model Performance
    valid_pred <- predict(rf_model, valid_set)
    confusionMatrix(valid_pred, valid_set$classe)

## Feature Importance
    varImpPlot(rf_model)

## Predict on Test Data
    test_pred <- predict(rf_model, test_data)
    print(test_pred)
