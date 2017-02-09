# Exercise 3 - Logistic regression
## loading the data file
alc <- read.csv ("D:/PhD University of Helsinki/Open Data Science/IODS-project/Data/alc.csv", header = TRUE, sep = ",")
str(alc)
## printing column names
colnames(alc)
## The data set consists of 35 variables with 382 participants. It consists of different information about students in math and Portuguese language courses (see names of the columns). 

## I will study the relationship between alcohol consumption and the following four variables 
## 1. sex 2. age 3. Pstatus (the cohabitation status of students' parents) 4. romantic (whether the student is in a romantic relationship or not)
## My hypotheses are as follows:
## 1. Boys are more likely to be heavy alcohol users than girls.
## 2. Older students drink more than younger students.
## 3. Students, whose parents live apart drink more than students whose parents live together.
## 4. Student in a romantic relationship drink less than students without a romantic relationship
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2", dependencies = c("Depends", "Imports", "Suggests"))
library(ggplot2)

install.packages("gmodels")
library(gmodels)

# sex vs high_use
sex_high_use <- CrossTable(alc$sex, alc$high_use)
g1 <- ggplot(alc, aes(x = sex, y = alc_use))
g1 + geom_boxplot() + ylab("alc_use")
# While 39.1% of the boys show high alcohol use, only 21.2% of the girls do. 63% of the high alcohol users are male while the total male share of the sample is just 48%.
# The boxplot shows that the CIs for males and females overlap largely. On average, males drink more than females.
# The hypotheses is supported by the data. Males drink more on average and are more likely to be high users than females.

# age vs high_use
g2 <- ggplot(alc, aes(x = high_use, y = age))
g2 + geom_boxplot() + ylab ("age")

reg1 <- lm (alc$alc_use ~ alc$age)
g3 <- plot(alc$alc_use ~ alc$age)
abline (reg1)
# Judging from the boxplot, high alcohol users are older than non-high-users on average. However, the confidence intervals overlap largely, indicating that there is no significant age difference between high alcohol users and non high alcohol users.
# The scatterplot alc_use vs age indicates a positive correlation between age and alcohol use. 
# However, the variable alc_use is not truly numeric but rather categorical (even though it says differently in the description of the dataset).
# Therefore, the correlation doesn't make much sense.

# Pstatus vs high_use
CrossTable(alc$Pstatus, alc$high_use)

g4 <- ggplot(alc, aes(x = Pstatus, y = alc_use))
g4 + geom_boxplot() + ylab("alc_use")

# 32% of students whose parents live apart are high alcohol users compared to 30% of students whose parents live together are high alcohol users.
# This indicates no large differences in high alcohol use depending on the parental cohabitation status.
# In addition, we can see in the boxplot that while the average alcohol consumption is slightly higher for the "together" group, the distributions are very similar in both groups, indicating no differences.
# The hypothesis is not supported.

# romantic vs high_use
CrossTable(alc$romantic, alc$high_use)
g5 <- ggplot(alc, aes(x = romantic, y = alc_use))
g5 + geom_boxplot() + ylab("alc_use")
# 31% of students who are in a romantic relationship have high alcohol use.
# 27% of students who are not in a romantic relationship have high alcohol use.
# This indicates that there is a small difference between people with or without a romantic relationship regarding high alcohol use.
# However, when we look at the boxplot, we see that alcohol consumption, the relationship status makes no difference. The distributions are the same.
# In total, there seems to be no difference in alcohol consumption between students with and without romantic relationships.

# Logistic regression
m <- glm (high_use ~ sex + age + Pstatus + romantic, data = alc, family = "binomial")
summary (m)
coef(m)
# The logistic regression model shows significant estimates for sex (0.88) and age (0.23) while Pstatus and romantic don't have a signficant influence on high_use.
# This indicates that men are more likely to be high alcohol users than women and that higher age increases the chances to be a high alcohol user

# Odds-Ratios with Confidence Intervals
OR <- coef(m)%>%exp
confint(m)
CI <- confint(m) %>% exp
cbind (OR, CI)
# The Odds Ratio for sex is 2.4, indicating that being male increases the chance to be a high alcohol user by factor 2.4 (CI: 1.5 - 3.8)
# The Odds Ratio for age is 1.3, indicating that every year of age in this sample increases the chance to be a high alcohol user by factor 1.3 (CI: 1.0 - 1.5)
# The Odds Ratio for Pstatus is 0.82, indicating that having parents who live together change the chance to be a high alcohol user by factor 0.8.However, since the CI includes 1 and we have seen in the logistic regression model that this variable was not a significant predictor, this is not trustworthy - the variation from 1 is probably by chance.
# The same is true for whether someone is in a romantic relationship or not (OR:0.8, CI: 0.5 - 1.3).

# Here, I examine how well the model actually predicts high alcohol use.
probabilities <- predict(m, type = "response")

alc <- mutate(alc, probability = probabilities)

alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)
# Seems like it's very conservative and not very good: 110 cases have a prediction of "FALSE" while the actual value is "TRUE".

# Let's take a closer look at the significant predictors from the model: sex and age
m1 <- glm(high_use ~ sex + age, data = alc, family = "binomial")
probabilities <- predict(m1, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)
table(high_use = alc$high_use, prediction = alc$prediction)
# We can see that this model is - in absolute numbers - worse than the one with more predictors (which is trivial)
# The number of correct "TRUE" predictions is now only 3 instead of 4.
# This plot illustrates the inaccuracy of the model prediction:
g_m1 <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))
g_m1 + geom_point()


table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = alc$high_use, prob = alc$probability)
# In this table and with the help of the loss function we can see that in total ~ 30% of all predictions were incorrect, mostly high users falsely predicted as non high users.
# Of the remaining 70% correctly classified cases, almost all are correctly classified as "FALSE".
# The model is very conservative, avoiding false alarms.
# Almost 30% of all students are high users. If we had predicted everyone as non high user, the proportion of wrong predictions would have been very similar to what we obtained with the model.
# The model is therefore not very predictive.

# Cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m1, K = 10)
cv$delta[1]
# The prediction error using 10-fold cross-validation is 0.31.
# This model is worse than the one introduced in Datcamp since the value is (slightly) higher, indicating that more wrong predictions were made.

# Is it possible to find a better model?
# By throwing in more predictors, we will surely get a better prediction.
m2 <- glm (high_use ~ age + sex + famsize + Medu + Fedu + studytime + failures + higher + paid + activities + famrel + freetime + goout, data = alc, family = "binomial")

probabilities <- predict(m2, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = alc$high_use, prob = alc$probability)
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m2, K = 10)
cv$delta[1]

# By adding all these variables, we decreased the loss function to .22 and the average amount of wrong predictions in the 10-fold cross-validation to 0.23, a value better than in the Datacamp exercise.

# As we can see here, some of these predictors are not significant:
summary(m2)

# Let's eliminate "higher" first since it has the lowest z value.
m3 <- glm (high_use ~ age + sex + famsize + Medu + Fedu + studytime + failures + paid + activities + famrel + freetime + goout, data = alc, family = "binomial")
summary (m3)
probabilities <- predict(m3, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = alc$high_use, prob = alc$probability)
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m3, K = 10)
cv$delta[1]
# We can see that the training error is 0.22 and the mean prediction error in the cross validation is 0.24.
# The model prediction didn't really get worse by eliminating "higher" from the model.

# Next-lowest predictor is "freetime". Let's try eliminating this as well.
m4 <- glm (high_use ~ age + sex + famsize + Medu + Fedu + studytime + failures + paid + activities + famrel + goout, data = alc, family = "binomial")
summary (m4)
probabilities <- predict(m4, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = alc$high_use, prob = alc$probability)
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m4, K = 10)
cv$delta[1]
# Our analysis here shows that the loss function is 0.22 and the prediction error in the cross-validation is .24 (rounded values).
# Interestingly, the absolute number of correct predictions has *increased* by reducing the amount of predictors.

# Next predictor to go is "Medu"
m5 <- glm (high_use ~ age + sex + famsize + Fedu + studytime + failures + paid + activities + famrel + goout, data = alc, family = "binomial")
summary (m5)
probabilities <- predict(m5, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = alc$high_use, prob = alc$probability)
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m5, K = 10)
cv$delta[1]
# Our analysis here shows that the loss function is 0.22 and the prediction error in the cross-validation is .22 (rounded values).
# Interestingly, quality of prediction has *increased* by reducing the amount of predictors again.