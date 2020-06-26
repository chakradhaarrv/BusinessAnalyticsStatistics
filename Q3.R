library(tidyverse)
library(ISLR)
data <- Weekly
WeeklyTrn<-data %>% filter(Year < 2009)
WeeklyTst<-data %>% filter(Year >= 2009)


require(ISLR); require(ggthemes);
require(GGally); require(knitr); require(broom);
require(kableExtra)
set.seed(1)
theme_set(theme_tufte(base_size = 14))


library(ggplot2)
library(caret)
#names(Weekly)
#head(Weekly)
#tail(Weekly)
#summary(Weekly)


logreg_Trn <- glm(Direction ~ . - Year - Today, data = WeeklyTrn, family = 'binomial')
summary(logreg_Trn)
#Performance Evaluation
predTrn <- predict(logreg_Trn,WeeklyTrn,type = "response")
predTrn_values <- ifelse(predTrn >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc <- paste('Accuracy:', mean(predTrn_values == WeeklyTrn$Direction))
kable(table(predTrn_values, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc)


#TestData Perf Eva
predTst <- predict(logreg_Trn, WeeklyTst, type = "response")
predTst_values <- ifelse(predTst >= 0.5, 'Up', 'Down')
summary(predTst)

#table(predTst_values, WeeklyTst$Direction)
accTst <- paste('Accuracy:', mean(predTst_values == WeeklyTst$Direction))
kable(table(predTst_values, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc)

#ROC plot
library(ROCR)

pr <- prediction(predTst, WeeklyTst$Direction)
prf <- performance(pr, "tpr", "fpr")
plot(prf)
abline(a=0, b=1)

auc <- performance(pr, "auc")
auc <- auc@y.values[[1]]
auc

# Logistic regression using lag1 variable ----

logreg_Lag1 <- glm(Direction ~ Lag1, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag1)
#Performance Evaluation
predTrn_L1 <- predict(logreg_Lag1,WeeklyTrn,type = "response")
predTrn_values_L1 <- ifelse(predTrn_L1 >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc_L1 <- paste('Accuracy:', mean(predTrn_values_L1 == WeeklyTrn$Direction))
kable(table(predTrn_values_L1, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L1)


#TestData Perf Eva
predTst_L1 <- predict(logreg_Lag1, WeeklyTst, type = "response")
predTst_values_L1 <- ifelse(predTst_L1 >= 0.5, 'Up', 'Down')
summary(predTst_L1)

#table(predTst_values, WeeklyTst$Direction)
acc_L1Tst <- paste('Accuracy:', mean(predTst_values_L1 == WeeklyTst$Direction))
kable(table(predTst_values_L1, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L1Tst)

#ROC plot
library(ROCR)

pr_L1 <- prediction(predTst_L1, WeeklyTst$Direction)
prf_L1 <- performance(pr_L1, "tpr", "fpr")
plot(prf_L1)
abline(a=0, b=1)

auc_L1 <- performance(pr_L1, "auc")
auc_L1 <- auc_L1@y.values[[1]]
auc_L1


# Logistic regression using lag2 variable ----

logreg_Lag2 <- glm(Direction ~ Lag2, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag2)
#Performance Evaluation
predTrn_L2 <- predict(logreg_Lag2,WeeklyTrn,type = "response")
predTrn_values_L2 <- ifelse(predTrn_L2 >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc_L2 <- paste('Accuracy:', mean(predTrn_values_L2 == WeeklyTrn$Direction))
kable(table(predTrn_values_L2, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L2)


#TestData Perf Eva
predTst_L2 <- predict(logreg_Lag2, WeeklyTst, type = "response")
predTst_values_L2 <- ifelse(predTst_L2 >= 0.5, 'Up', 'Down')
summary(predTst_L2)

#table(predTst_values, WeeklyTst$Direction)
acc_L2Tst <- paste('Accuracy:', mean(predTst_values_L2 == WeeklyTst$Direction))
kable(table(predTst_values_L2, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L2Tst)

#ROC plot
library(ROCR)

pr_L2 <- prediction(predTst_L2, WeeklyTst$Direction)
prf_L2 <- performance(pr_L2, "tpr", "fpr")
plot(prf_L2)
abline(a=0, b=1)

auc_L2 <- performance(pr_L2, "auc")
auc_L2 <- auc_L2@y.values[[1]]
auc_L2


# Logistic regression using lag3 variable ----

logreg_Lag3 <- glm(Direction ~ Lag3, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag3)
#Performance Evaluation
predTrn_L3 <- predict(logreg_Lag3,WeeklyTrn,type = "response")
predTrn_values_L3 <- ifelse(predTrn_L3 >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc_L3 <- paste('Accuracy:', mean(predTrn_values_L3 == WeeklyTrn$Direction))
kable(table(predTrn_values_L3, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L3)


#TestData Perf Eva
predTst_L3 <- predict(logreg_Lag3, WeeklyTst, type = "response")
predTst_values_L3 <- ifelse(predTst_L3 >= 0.5, 'Up', 'Down')
summary(predTst_L3)

#table(predTst_values, WeeklyTst$Direction)
acc_L3Tst <- paste('Accuracy:', mean(predTst_values_L3 == WeeklyTst$Direction))
kable(table(predTst_values_L3, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L3Tst)

#ROC plot
library(ROCR)

pr_L3 <- prediction(predTst_L3, WeeklyTst$Direction)
prf_L3 <- performance(pr_L3, "tpr", "fpr")
plot(prf_L3)
abline(a=0, b=1)

auc_L3 <- performance(pr_L3, "auc")
auc_L3 <- auc_L3@y.values[[1]]
auc_L3


# Logistic regression using lag4 variable ----

logreg_Lag4 <- glm(Direction ~ Lag4, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag4)
#Performance Evaluation
predTrn_L4 <- predict(logreg_Lag4,WeeklyTrn,type = "response")
predTrn_values_L4 <- ifelse(predTrn_L4 >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc_L4 <- paste('Accuracy:', mean(predTrn_values_L4 == WeeklyTrn$Direction))
kable(table(predTrn_values_L4, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L5)


#TestData Perf Eva
predTst_L4 <- predict(logreg_Lag4, WeeklyTst, type = "response")
predTst_values_L4 <- ifelse(predTst_L4 >= 0.5, 'Up', 'Down')
summary(predTst_L4)

#table(predTst_values, WeeklyTst$Direction)
acc_L4Tst <- paste('Accuracy:', mean(predTst_values_L4 == WeeklyTst$Direction))
kable(table(predTst_values_L4, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L4Tst)

#ROC plot
library(ROCR)

pr_L4 <- prediction(predTst_L4, WeeklyTst$Direction)
prf_L4 <- performance(pr_L4, "tpr", "fpr")
plot(prf_L4)
abline(a=0, b=1)

auc_L4 <- performance(pr_L4, "auc")
auc_L4 <- auc_L4@y.values[[1]]
auc_L4


# Logistic regression using lag5 variable ----

logreg_Lag5 <- glm(Direction ~ Lag5, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag5)
#Performance Evaluation
predTrn_L5 <- predict(logreg_Lag5,WeeklyTrn,type = "response")
predTrn_values_L5 <- ifelse(predTrn_L5 >= 0.5, 'Up', 'Down')

#table(predTrn_values, WeeklyTrn$Direction)
acc_L5 <- paste('Accuracy:', mean(predTrn_values_L5 == WeeklyTrn$Direction))
kable(table(predTrn_values_L5, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L5)


#TestData Perf Eva
predTst_L5 <- predict(logreg_Lag5, WeeklyTst, type = "response")
predTst_values_L5 <- ifelse(predTst_L5 >= 0.5, 'Up', 'Down')
summary(predTst_L5)

#table(predTst_values, WeeklyTst$Direction)
acc_L5Tst <- paste('Accuracy:', mean(predTst_values_L5 == WeeklyTst$Direction))
kable(table(predTst_values_L5, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc_L5Tst)

#ROC plot
library(ROCR)

pr_L5 <- prediction(predTst_L5, WeeklyTst$Direction)
prf_L5 <- performance(pr_L5, "tpr", "fpr")
plot(prf_L5)
abline(a=0, b=1)

auc_L5 <- performance(pr_L5, "auc")
auc_L5 <- auc_L5@y.values[[1]]
auc_L5




# ROC and PR Curves ----
#########
abline(a=0, b= 1)
plot(prf, col='red')
plot(prf_L1, col='black', add=TRUE)
plot(prf_L2, col='blue', add=TRUE)
plot(prf_L3, col='green', add=TRUE)
plot(prf_L4, col='yellow', add=TRUE)
plot(prf_L5, col='orange', add=TRUE)
legend('bottomright', c('All', 'Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5'), lty=1, col=c('red', 'black', 'blue', 'green', 'yellow', 'Orange'))
#########

#Calculate Score:
WeeklyTst$Score <- predTst

dat<-WeeklyTst %>% select(-c(Lag1, Lag2, Lag3, Lag4, Lag5, Year, Today, Volume))

#Precision-Recall Curve

pr_dat <- dat %>% pr.curve(Direction, Score)
pr<-pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)

print(dat)

  
pr_dat %>%
  arrange(.threshold) %>% # this step is not strictly necessary here because the rows are already ordered by `.threshold`
  ggplot() +
  geom_path(aes(recall, precision)) + # connect the points in the order in which they appear in the data to form a curve
  coord_equal()


