---
title: "math448_project"
author: "Samantha Bell"
date: "2025-09-16"
output: html_document
---

# 1. reading data into R
```{r}
library(readxl)
library(dplyr)
library(MASS)
library(pROC)
```

```{r}
pcos = read.csv("C:/Users/theresa/Downloads/pcos_dataset.csv") 
```
```{r}
# transforming variables
pcos = pcos |>
  mutate(PCOS_Diagnosis = factor(PCOS_Diagnosis)) |>
  mutate(Menstrual_Irregularity = as.numeric(factor(Menstrual_Irregularity))) |>
  mutate(Age = as.numeric(Age)) |>
  mutate(Antral_Follicle_Count = as.numeric(Antral_Follicle_Count))
```


# 2. processing data

## checking for NA's
```{r}
sum(is.na(pcos))
```

## range of variables
```{r}
t(sapply(pcos[,1:5], range))
```

## checking for balance
```{r}
t = table(pcos$PCOS_Diagnosis)
t 
```

```{r}
pie(c(801,199), labels = c('no diagnosis', 'yes diagnosis'))
```

### The data I will be using is compiled of 1000 patients and five variables which are typically associated with PCOS diagnosis and its risks. These variables are age, BMI, menstrual irregularity, testosterone level, and number of antral follicles. Finally, the response variable is whether a patient has been diagnosed with PCOS or not, making it a classification data set.

### After running a few tests, I see that I have no missing data throughout the 1000 entries and 6 variables. I do, however, have unbalanced data, 80.1% with no diagnosis and 19.9% with a diagnosis. Having such a large unbalance in data can lead to biased outcomes when trying to model the data.

## balancing data
```{r}
library(ROSE)
set.seed(1)
pcos = ovun.sample(PCOS_Diagnosis ~ ., data=pcos, method="under")$data
t = table(pcos$PCOS_Diagnosis)
t
```

```{r}
pie(c(183,199), labels = c('no diagnosis', 'yes diagnosis'))
```

```{r}
t(sapply(pcos[,1:5], range))
```
### Now I have a more balanced data with only 382 entries, 183 with no diagnosis and 199 with a diagnosis. Also, the range of the cut-down data is the same as the original. So, this data will be a great sample that does not lead to bias.

# 3. Descriptive Analysis

```{r}
library(ggplot2)

ggplot(pcos, aes(x= as.factor(PCOS_Diagnosis), y = Age)) +
  geom_boxplot()

ggplot(pcos, aes(x= as.factor(PCOS_Diagnosis), y = BMI)) +
  geom_boxplot()

ggplot(pcos, aes(x = Menstrual_Irregularity, fill = as.factor(PCOS_Diagnosis))) +
  geom_bar(position = 'fill')

 ggplot(pcos, aes(x= as.factor(PCOS_Diagnosis), y = Testosterone_Level.ng.dL.)) +
  geom_boxplot()

ggplot(pcos, aes(x= as.factor(PCOS_Diagnosis), y = Antral_Follicle_Count)) +
  geom_boxplot()

```

```{r}
fit = glm(PCOS_Diagnosis ~ ., data = pcos, family = 'binomial')
summary(fit)
```
# backward and forward step
```{r}
backward_model <- stepAIC(fit, direction = "backward")
```




```{r}
null = glm(PCOS_Diagnosis ~ 1, data = pcos, family = 'binomial')
forward_model = stepAIC(null, scope = list(lower= null, upper = fit),direction = 'forward')
```

# validation-set approach
```{r}
# training is 1/2 data
set.seed(10)
train = sample(nrow(pcos), size = nrow(pcos)/2)

fit_train = glm(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL.+ Antral_Follicle_Count, data = pcos, subset = train, family = 'binomial')

p = ifelse(predict(fit_train, newdata = pcos[-train,], type = 'response') > 0.5, 'yes', 'no')
t = table(p, pcos$PCOS_Diagnosis[-train])

(sum(diag(t))/sum(t))
```

```{r}
# training is 1/3 data
set.seed(10)
train = sample(nrow(pcos), size = nrow(pcos)/3)

fit_train = glm(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos, subset = train, family = 'binomial')

p = ifelse(predict(fit_train, newdata = pcos[-train,], type = 'response') > 0.5, 'yes', 'no')
t = table(p, pcos$PCOS_Diagnosis[-train])

(sum(diag(t))/sum(t))
```

```{r}
# training is 2/3 data
set.seed(10)
train = sample(nrow(pcos), size = nrow(pcos)*(2/3))

fit_train = glm(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos, subset = train, family = 'binomial')

p = ifelse(predict(fit_train, newdata = pcos[-train,], type = 'response') > 0.5, 'yes', 'no')
t = table(p, pcos$PCOS_Diagnosis[-train])

(sum(diag(t))/sum(t))
```

```{r}
# splitting data 
pcos_train = pcos[train,]
pcos_test = pcos[-train,]
```


# log regression
```{r}
fit = glm(PCOS_Diagnosis ~  BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train, family = 'binomial')

# accuracy
pred = predict(fit, newdata = pcos_test, type = 'response')
p = ifelse(pred > 0.5, 1, 0)
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(fit_acc = sum(diag(t))/sum(t))

# auc
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)
```

# LDA
```{r}
ldafit = lda(PCOS_Diagnosis ~  BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train)
#plot(ldafit)

# accuracy
p = predict(ldafit, newdata = pcos_test)$class
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(lda_acc = sum(diag(t))/sum(t))

# auc
pred = as.numeric(p)
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)
```


# QDA
```{r}
#qdafit = qda(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train)

#acc(qdafit)

# not working!!!
```

# decision tree
```{r}
library(rpart.plot)
rtree = rpart(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train, cp = 0.008)
rpart.plot(rtree)
#text(tree, cex = 0.5, digits = 3)
```

```{r}
library(tree)
tree = tree(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train)
plot(tree)
text(tree, cex = 0.5, digits = 3)
```
```{r}
# accuracy
p = predict(tree, newdata = pcos_test, type = 'class')
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(tree_acc = sum(diag(t))/sum(t))

# auc
pred = as.numeric(p)
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)

```

```{r}
# pruned
library(tree)
cv = cv.tree(tree)

plot(cv$size, cv$dev, type = 'b', xlab = 'tree size')
min = which.min(cv$dev)
abline(v = cv$size[min], lty = 2, col = 'red')
```


# random forest
```{r}
library(randomForest)
```
```{r}
rf = randomForest(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train, mtry = 2)

# accuracy
p = predict(rf, newdata = pcos_test, type = 'class')
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(rf_acc = sum(diag(t))/sum(t))

# auc
pred = as.numeric(p)
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)
```


# bagging
```{r}
bag = randomForest(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train, mtry = 4)

p = predict(bag, newdata = pcos_test, type = 'class')
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(bag_acc = sum(diag(t))/sum(t))

# auc
pred = as.numeric(p)
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)
```


# naive bayes
```{r}
library(e1071)
library()
nbfit = naiveBayes(PCOS_Diagnosis ~ BMI + Menstrual_Irregularity + Testosterone_Level.ng.dL. + Antral_Follicle_Count, data = pcos_train)

p = predict(nbfit, newdata = pcos_test, type = 'class')
t = table(p, pcos_test$PCOS_Diagnosis)
print(t)
(nb_acc = sum(diag(t))/sum(t))

# auc
pred = as.numeric(p)
plot(roc(pcos_test$PCOS_Diagnosis, pred), print.auc = TRUE)
```

```{r}
library(tidyr)
table = data.frame(model = c('log reg', 'lda', 'tree', 'rand forest', 'bagging', 'naive bayes'), 
                     accuracy = c(fit_acc, lda_acc, tree_acc, rf_acc, bag_acc, nb_acc),
                     auc = c(0.982, 0.9, 0.992, 0.992, 0.992, 0.769))
table2 = table |>
  gather('p', 'value', -model)

ggplot(table2) + geom_col(aes(x = model, y = value, fill = p), position = 'dodge') + scale_fill_manual(values = c('brown','pink'))
```
