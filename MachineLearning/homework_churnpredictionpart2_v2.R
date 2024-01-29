rm(list = ls())

# load libraries
library(tidyverse)
library(caret)
library(corrplot)
library(pROC)
library(MLmetrics)
library(caTools)
library(gbm3)
library(xgboost)
library(lightgbm)

## load data
df <- read_csv("churn.csv")

## preview data
head(df)

# data summary
summary(df)

# missing values
sum(is.na(df))

# convert binary categorical variables to numeric for correlation analysis
df$internationalplan <- ifelse(df$internationalplan == "yes", 1, 0)
df$voicemailplan <- ifelse(df$voicemailplan == "yes", 1, 0)
df$churn <- ifelse(df$churn == "Yes", 1, 0)

# compute the correlation matrix
corr_matrix <- cor(df[sapply(df, is.numeric)])

# plot the correlation matrix
corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.6)

# visualize the relationship of categorical variables with churn
# internationalplan vs Churn
ggplot(df, aes(x = internationalplan, fill = as.factor(churn))) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(title = "Churn Distribution by International Plan", x = "International Plan", y = "Proportion")

# voicemailplan vs Churn
ggplot(df, aes(x = voicemailplan, fill = as.factor(churn))) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(title = "Churn Distribution by Voicemail Plan", x = "Voicemail Plan", y = "Proportion")

## convert 'churn' to a factor with two levels for classification modeling
df$churn <- factor(ifelse(df$churn == 1, "Yes", "No"), levels = c("No", "Yes"))

'''
select features
internationalplan, voicemailplan, totaldaycharge, numbercustomerservicecalls

avoid multicollinearity of
1) voicemailplan vs. numbervmailmessages
2) totaldayminutes vs. totaldaycharge
3) totaleveminutes vs. totalevecharge
4) totalnightminutes vs. totalnightcarge
5) totalintlminutes vs. totalintlcharge
'''

# original 4 predictors formula
model <- train(churn ~ internationalplan + voicemailplan + totaldaycharge + servicecalls_ordinal)

## load data
df <- read_csv("churn.csv")

# create new ordinal variable for numbercustomerservicecalls
cutoffs <- c(0, 2, 4, Inf) # Define the cutoffs
df$servicecalls_ordinal <- cut(df$numbercustomerservicecalls,
                               breaks = cutoffs,
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE)

# ensure 'churn' and other categorical variables to factors in the original dataset
df$churn <- factor(df$churn, levels = c("No", "Yes"))
df$internationalplan <- factor(df$internationalplan)
df$voicemailplan <- factor(df$voicemailplan)
df$servicecalls_ordinal <- factor(df$servicecalls_ordinal)

## 1. split data
set.seed(42)
split <- sample.split(df$churn, SplitRatio = 0.75)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

## preprocess the data
preProc <- preProcess(train, method = c("center", "scale"))

# apply pre-processing to train and test data
train_z <- predict(preProc, train)
test_z <- predict(preProc, test)

## 2. train model

## set control loop
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary, #prSummary
                     classProbs = TRUE,
                     #sampling = "up") # up-sampling for imbalanced classes
                     verboseIter = FALSE)

# Logistic Regression
glm_model <- train(churn ~ .,
                   data = train_z,
                   method = "glm",
                   family = "binomial",
                   trControl = ctrl,
                   metric = "ROC")

# K-Nearest Neighbors

tuneGrid <- expand.grid(k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25))
                        #weight = c("uniform", "distance"),
                        #metric = c("euclidean", "manhattan")

knn_model <- train(churn ~ .,
                   data = train_z,
                   method = "knn",
                   tuneGrid = tuneGrid,
                   trControl = ctrl,
                   metric = "ROC")

# Decision Tree
tree_model <- train(churn ~ .,
                   data = train_z,
                   method = "rpart",
                   trControl = ctrl,
                   metric = "ROC")

# Gradient Boost with gbm3
gbm_model <- gbm(churn ~ ., 
                 data = train_z, 
                 distribution = "bernoulli", 
                 n.trees = 500,  # Increased number of trees
                 interaction.depth = 4,  # Adjusted depth
                 shrinkage = 0.01,  # Adjusted learning rate
                 cv.folds = 10,
                 keep.data = TRUE, 
                 verbose = FALSE)

# Extreme Gradient Boost with xgboost
# One-hot encoding categorical variables
X_train_xgb <- model.matrix(~ . - 1, data = train_z)
X_test_xgb <- model.matrix(~ . - 1, data = test_z)

# Prepare labels for xgboost
labels <- as.numeric(train_z$churn) - 1  # xgboost expects 0/1 labels

# Train the xgboost model
xgb_model <- xgboost(data = X_train_xgb, 
                     label = labels, 
                     nrounds = 100, 
                     objective = "binary:logistic",
                     verbose = FALSE)

## 3. score model
#pred <- predict(model, newdata = test_z)
#cm <- confusionMatrix(pred, test_z$churn)

## 4. Evaluate models
evaluate_model <- function(model, test, model_type = "caret") {
  if (model_type == "caret") {
    pred <- predict(model, newdata = test)
    prob_predictions <- predict(model, newdata = test, type = "prob")[, "Yes"]
  } else if (model_type == "xgboost") {
    # Convert test data to a numeric matrix for xgboost
    test_data_matrix <- model.matrix(~ . - 1, data = test)
    pred_prob <- predict(model, test_data_matrix)
    pred <- ifelse(pred_prob > 0.5, "Yes", "No")
    prob_predictions <- pred_prob
  } else if (model_type == "gbm3") {
    pred_prob <- predict(model, newdata = test, n.trees = 100, type = "response")
    pred <- ifelse(pred_prob > 0.5, "Yes", "No")
    prob_predictions <- pred_prob
  }
  pred <- factor(pred, levels = levels(test$churn))
  cm <- confusionMatrix(pred, test$churn)
  roc_obj <- roc(response = test$churn, predictor = prob_predictions)
  auc_value <- auc(roc_obj)
  return(list(confusion_matrix = cm, AUC = auc_value))
}

eval_glm <- evaluate_model(glm_model, test_z)
eval_knn <- evaluate_model(knn_model, test_z)
eval_tree <- evaluate_model(tree_model, test_z)
eval_gbm <- evaluate_model(gbm_model, test_z, model_type = "gbm3")
eval_xgb <- evaluate_model(xgb_model, test_z, model_type = "xgboost")

# Compare AUC values to find the best model
auc_values <- c(GLM = eval_glm$AUC,
                KNN = eval_knn$AUC,
                Tree = eval_tree$AUC)
                #GBM = eval_gbm$AUC
                #XGB = eval_xgb$AUC

best_model <- names(which.max(auc_values))
cat("The best model based on AUC is:", best_model, "with AUC of", max(auc_values), "\n")


# GBM Hyperparameter tuning
library(h2o)
h2o.init()

## Convert data to h2o format
train_h2o <- as.h2o(train_z)
y <- "churn"
x <- setdiff(names(train_z), y)

## Define the hyperparameter grid
hyper_params <- list(max_depth = c(3, 5, 9),
                     ntrees = c(50, 100, 200),
                     learn_rate = c(0.01, 0.1))

## Search criteria can be Grid, Random or Cartesian
search_criteria <- list(strategy = "RandomDiscrete", max_models = 20, seed = 1)

## Train the model
gbm_grid <- h2o.grid("gbm", x = x, y = y, training_frame = train_h2o,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

