rm(list = ls())

# load libraries
library(tidyverse)
library(caret)
library(corrplot)
library(reshape2)
library(pROC)

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

# pre-processing
## convert 'churn' to a factor with two levels for classification modeling
df$churn <- factor(ifelse(df$churn == 1, "Yes", "No"), levels = c("No", "Yes"))

# create new ordinal variable for numbercustomerservicecalls
cutoffs <- c(0, 2, 4, Inf) # Define the cutoffs
df$servicecalls_ordinal <- cut(df$numbercustomerservicecalls, breaks = cutoffs, labels = c("Low", "Medium", "High"), include.lowest = TRUE)

## 1. split data
train_test_split <- function(data, size=0.8) {
  set.seed(42)
  n <- nrow(data)
  train_id <- sample(1:n, size*n)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return( list(train_df, test_df) )
}

prep_df <- train_test_split(df, size=0.7)

## 2. train model
ctrl <- trainControl(method = "cv",
                     number = 5)

model <- train(churn ~ internationalplan + voicemailplan + totaldaycharge + servicecalls_ordinal,
               data = prep_df[[1]],
               method = "glm",
               family = "binomial",
               trControl = ctrl)

## 3. score model
pred_churn <- predict(model, newdata= prep_df[[2]])

## 4. evaluate model
actual_churn <- prep_df[[2]]$churn

cm <- confusionMatrix(pred_churn, actual_churn)

model
cm

# optionally, calculate AUC
prob_predictions <- predict(model, newdata = prep_df[[2]], type = "prob")
roc_obj <- roc(response = as.factor(actual_churn), predictor = prob_predictions[,"Yes"])
auc_value <- auc(roc_obj)
print(auc_value)