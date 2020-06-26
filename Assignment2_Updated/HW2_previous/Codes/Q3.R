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
cm <- as.matrix(table(predTrn_values, WeeklyTrn$Direction))

###############

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm[2,2]
FP = cm[2,1]
FN = cm[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

###########

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

# Precision, Recall and FScore ----
cmt_a <- as.matrix(table(predTst_values, WeeklyTst$Direction))
cmt_a

n = sum(cmt_a) # number of instances
nc = nrow(cmt_a) # number of classes
diag = diag(cmt_a) # number of correctly classified instances per class 
rowsums = apply(cmt_a, 1, sum) # number of instances per class
colsums = apply(cmt_a, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_a[2,2]
FP = cmt_a[2,1]
FN = cmt_a[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

##########

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
prf_1 <- performance(pr, "prec", "rec")
plot(prf_1)
plot(prf)
abline(a=0, b=1)

auc <- performance(pr, "auc")
auc <- auc@y.values[[1]]
auc

WeeklyTst$Direction <- ifelse(WeeklyTst$Direction == "Up",1,0)

fg <- predTst[WeeklyTst$Direction == 1]
bg <- predTst[WeeklyTst$Direction == 0]


pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

# Logistic regression using lag1 variable ----

logreg_Lag1 <- glm(Direction ~ Lag1, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag1)
#Performance Evaluation
predTrn_L1 <- predict(logreg_Lag1,WeeklyTrn,type = "response")
predTrn_values_L1 <- ifelse(predTrn_L1 >= 0.5, 'Up', 'Down')


# Precision, Recall and FScore ----
cm_1 <- as.matrix(table(predTrn_values_L1, WeeklyTrn$Direction))
cm_1

n = sum(cm_1) # number of instances
nc = nrow(cm_1) # number of classes
diag = diag(cm_1) # number of correctly classified instances per class 
rowsums = apply(cm_1, 1, sum) # number of instances per class
colsums = apply(cm_1, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm_1[2,2]
FP = cm_1[2,1]
FN = cm_1[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTrn_values_L1, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)


#TestData Perf Eva
predTst_L1 <- predict(logreg_Lag1, WeeklyTst, type = "response")
predTst_values_L1 <- ifelse(predTst_L1 >= 0.5, 'Up', 'Down')
summary(predTst_L1)

#TEST_LAG1
cmt_1 <- as.matrix(table(predTst_values_L1, WeeklyTst$Direction))
cmt_1

n = sum(cmt_1) # number of instances
nc = nrow(cmt_1) # number of classes
diag = diag(cmt_1) # number of correctly classified instances per class 
rowsums = apply(cmt_1, 1, sum) # number of instances per class
colsums = apply(cmt_1, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_1[2,2]
FP = cmt_1[2,1]
FN = cmt_1[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy


kable(table(predTst_values_L1, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)

#ROC plot
library(ROCR)

pr_L1 <- prediction(predTst_L1, WeeklyTst$Direction)
prf_L1 <- performance(pr_L1, "tpr", "fpr")
plot(prf_L1)
abline(a=0, b=1)

prf1_1 <- performance(pr_L1, "prec", "rec")


auc_L1 <- performance(pr_L1, "auc")
auc_L1 <- auc_L1@y.values[[1]]
auc_L1

fg <- predTst_L1[WeeklyTst$Direction == 1]
bg <- predTst_L1[WeeklyTst$Direction == 0]


prf1_1 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prf1_1)


# Logistic regression using lag2 variable ----

logreg_Lag2 <- glm(Direction ~ Lag2, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag2)
#Performance Evaluation
predTrn_L2 <- predict(logreg_Lag2,WeeklyTrn,type = "response")
predTrn_values_L2 <- ifelse(predTrn_L2 >= 0.5, 'Up', 'Down')

cm_2 <- as.matrix(table(predTrn_values_L2, WeeklyTrn$Direction))
cm_2

n = sum(cm_2) # number of instances
nc = nrow(cm_2) # number of classes
diag = diag(cm_2) # number of correctly classified instances per class 
rowsums = apply(cm_2, 1, sum) # number of instances per class
colsums = apply(cm_2, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm_2[2,2]
FP = cm_2[2,1]
FN = cm_2[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTrn_values_L2, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)

#TestData Perf Eva
predTst_L2 <- predict(logreg_Lag2, WeeklyTst, type = "response")
predTst_values_L2 <- ifelse(predTst_L2 >= 0.5, 'Up', 'Down')
summary(predTst_L2)

cmt_2 <- as.matrix(table(predTst_values_L2, WeeklyTst$Direction))
cmt_2

n = sum(cmt_2) # number of instances
nc = nrow(cmt_2) # number of classes
diag = diag(cmt_2) # number of correctly classified instances per class 
rowsums = apply(cmt_2, 1, sum) # number of instances per class
colsums = apply(cmt_2, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_2[2,2]
FP = cmt_2[2,1]
FN = cmt_2[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTst_values_L2, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) 

#ROC plot
library(ROCR)
library(pROC)

pr_L2 <- prediction(predTst_L2, WeeklyTst$Direction)
prf_L2 <- performance(pr_L2, "tpr", "fpr")
plot(prf_L2)
abline(a=0, b=1)

prf1_2 <- performance(pr_L2, "prec", "rec")

auc_L2 <- performance(pr_L2, "auc")
auc_L2 <- auc_L2@y.values[[1]]
auc_L2

WeeklyTst$Direction <- ifelse(WeeklyTst$Direction == "Up",1,0)

fg <- predTst_L2[WeeklyTst$Direction == 1]
bg <- predTst_L2[WeeklyTst$Direction == 0]


prf1_2 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prf1_2)

# Logistic regression using lag3 variable ----

logreg_Lag3 <- glm(Direction ~ Lag3, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag3)
#Performance Evaluation
predTrn_L3 <- predict(logreg_Lag3,WeeklyTrn,type = "response")
predTrn_values_L3 <- ifelse(predTrn_L3 >= 0.5, 'Up', 'Down')

cm_3 <- as.matrix(table(predTrn_values_L3, WeeklyTrn$Direction))
cm_3

n = sum(cm_3) # number of instances
nc = nrow(cm_3) # number of classes
diag = diag(cm_3) # number of correctly classified instances per class 
rowsums = apply(cm_3, 1, sum) # number of instances per class
colsums = apply(cm_3, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm_3[1,2]
FP = cm_3[1,1]
FN = 0 #cm_3[1,2]
accuracy = cm_3[1,2]/n 
precision =  TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTrn_values_L3, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)


#TestData Perf Eva
predTst_L3 <- predict(logreg_Lag3, WeeklyTst, type = "response")
predTst_values_L3 <- ifelse(predTst_L3 >= 0.5, 'Up', 'Down')
summary(predTst_L3)

cmt_3 <- as.matrix(table(predTst_values_L3, WeeklyTst$Direction))
cmt_3

n = sum(cmt_3) # number of instances
nc = nrow(cmt_3) # number of classes
diag = diag(cmt_3) # number of correctly classified instances per class 
rowsums = apply(cmt_3, 1, sum) # number of instances per class
colsums = apply(cmt_3, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_3[1,2]
FP = cmt_3[1,1]
FN = 0 #cmt_3[1,2]
accuracy = cmt_3[1,2]/n 
precision =  TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTst_values_L3, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)

#ROC plot
library(ROCR)

pr_L3 <- prediction(predTst_L3, WeeklyTst$Direction)
prf_L3 <- performance(pr_L3, "tpr", "fpr")
plot(prf_L3)
abline(a=0, b=1)

prf1_3 <- performance(pr_L3, "prec", "rec")

auc_L3 <- performance(pr_L3, "auc")
auc_L3 <- auc_L3@y.values[[1]]
auc_L3

fg <- predTst_L3[WeeklyTst$Direction == 1]
bg <- predTst_L3[WeeklyTst$Direction == 0]


prf1_3 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prf1_3)


# Logistic regression using lag4 variable ----

logreg_Lag4 <- glm(Direction ~ Lag4, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag4)
#Performance Evaluation
predTrn_L4 <- predict(logreg_Lag4,WeeklyTrn,type = "response")
predTrn_values_L4 <- ifelse(predTrn_L4 >= 0.5, 'Up', 'Down')

cm_4 <- as.matrix(table(predTrn_values_L4, WeeklyTrn$Direction))
cm_4

n = sum(cm_4) # number of instances
nc = nrow(cm_4) # number of classes
diag = diag(cm_4) # number of correctly classified instances per class 
rowsums = apply(cm_4, 1, sum) # number of instances per class
colsums = apply(cm_4, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm_4[1,2]
FP = cm_4[1,1]
FN = 0 #cm_4[1,2]
accuracy = cm_4[1,2]/n 
precision =  TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTrn_values_L4, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) 


#TestData Perf Eva
predTst_L4 <- predict(logreg_Lag4, WeeklyTst, type = "response")
predTst_values_L4 <- ifelse(predTst_L4 >= 0.5, 'Up', 'Down')
summary(predTst_L4)

cmt_4 <- as.matrix(table(predTst_values_L4, WeeklyTst$Direction))
cmt_4

n = sum(cmt_4) # number of instances
nc = nrow(cmt_4) # number of classes
diag = diag(cmt_4) # number of correctly classified instances per class 
rowsums = apply(cmt_4, 1, sum) # number of instances per class
colsums = apply(cmt_4, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_4[1,2]
FP = cmt_4[1,1]
FN = 0 #cmt_4[1,2]
accuracy = cmt_4[1,2]/n 
precision =  TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTst_values_L4, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)

#ROC plot
library(ROCR)

pr_L4 <- prediction(predTst_L4, WeeklyTst$Direction)
prf_L4 <- performance(pr_L4, "tpr", "fpr")
plot(prf_L4)
abline(a=0, b=1)

prf1_4 <- performance(pr_L4, "prec", "rec")

auc_L4 <- performance(pr_L4, "auc")
auc_L4 <- auc_L4@y.values[[1]]
auc_L4

fg <- predTst_L4[WeeklyTst$Direction == 1]
bg <- predTst_L4[WeeklyTst$Direction == 0]


prf1_4 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prf1_4)


# Logistic regression using lag5 variable ----

logreg_Lag5 <- glm(Direction ~ Lag5, data = WeeklyTrn, family = 'binomial')
summary(logreg_Lag5)
#Performance Evaluation
predTrn_L5 <- predict(logreg_Lag5,WeeklyTrn,type = "response")
predTrn_values_L5 <- ifelse(predTrn_L5 >= 0.5, 'Up', 'Down')

cm_5 <- as.matrix(table(predTrn_values_L5, WeeklyTrn$Direction))
cm_5

n = sum(cm_5) # number of instances
nc = nrow(cm_5) # number of classes
diag = diag(cm_5) # number of correctly classified instances per class 
rowsums = apply(cm_5, 1, sum) # number of instances per class
colsums = apply(cm_5, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cm_5[2,2]
FP = cm_5[2,1]
FN = cm_5[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTrn_values_L5, WeeklyTrn$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)


#TestData Perf Eva
predTst_L5 <- predict(logreg_Lag5, WeeklyTst, type = "response")
predTst_values_L5 <- ifelse(predTst_L5 >= 0.5, 'Up', 'Down')
summary(predTst_L5)

cmt_5 <- as.matrix(table(predTst_values_L5, WeeklyTst$Direction))
cmt_5
###### start from here
n = sum(cmt_5) # number of instances
nc = nrow(cmt_5) # number of classes
diag = diag(cmt_5) # number of correctly classified instances per class 
rowsums = apply(cmt_5, 1, sum) # number of instances per class
colsums = apply(cmt_5, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
TP = cmt_5[2,2]
FP = cmt_5[2,1]
FN = cmt_5[1,2]
accuracy = sum(diag) / n 
precision = TP / (TP + FP) #TP / (TP + FP)
recall = TP/(TP+FN) # TP/(TP+FN)
fscore = precision * recall
precision
recall
fscore
accuracy

kable(table(predTst_values_L5, WeeklyTst$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T)

#ROC plot
library(ROCR)

pr_L5 <- prediction(predTst_L5, WeeklyTst$Direction)
prf_L5 <- performance(pr_L5, "tpr", "fpr")
plot(prf_L5)
abline(a=0, b=1)

prf1_5 <- performance(pr_L5, "prec", "rec")


auc_L5 <- performance(pr_L5, "auc")
auc_L5 <- auc_L5@y.values[[1]]
auc_L5

fg <- predTst_L5[WeeklyTst$Direction == 1]
bg <- predTst_L5[WeeklyTst$Direction == 0]


prf1_5 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prf1_5)


3# ROC Curves ----


plot(prf, col='red')
plot(prf_L1, col='black', add=TRUE)
plot(prf_L2, col='blue', add=TRUE)
plot(prf_L3, col='green', add=TRUE)
plot(prf_L4, col='yellow', add=TRUE)
plot(prf_L5, col='orange', add=TRUE)
abline(a=0, b= 1)
legend('bottomright', c('All', 'Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5'), lty=1, col=c('red', 'black', 'blue', 'green', 'yellow', 'Orange'))

# PR Curves ----


#Precision-Recall Curve

prf_1 <- performance(pr, "prec", "rec")
plot(pr, col='red')
plot(prf1_1, col='black', add=TRUE)
plot(prf1_2, col='blue', add=TRUE)
plot(prf1_3, col='green', add=TRUE)
plot(prf1_4, col='yellow', add=TRUE)
plot(prf1_5, col='orange', add=TRUE)
legend('bottomright', c('All', 'Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5'), lty=1, col=c('red', 'black', 'blue', 'green', 'yellow', 'Orange'))

