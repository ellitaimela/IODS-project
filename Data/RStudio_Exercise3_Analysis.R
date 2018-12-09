# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 3
#
# Part 2: Analysis
#
# Elli Taimela
# 18.11.2018
# 
# This exercise analyses the relationship between Portuguese students' learning outcomes and alcohol consumption. 
# 

library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

# install.packages("tidyr")

# ---- 2 -----

# Reading the data
alc <- read.csv("alc.csv", sep=",")
# This data approach student achievement in secondary education of two Portuguese schools. 
# The data attributes include student grades, demographic, social and school related features) 
# and it was collected by using school reports and questionnaires. Two datasets are provided 
# regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por).

# Checking the data, and the structures, dimensions and summaries of the datasets
alc
str(alc)
dim(alc)
summary(alc)

# ---- 4 -----

# ggpairs(alc, lower = list(combo = wrap("facethist", bins = 20)))

# initialize a plot of high_use and family relationships
g1 <- ggplot(alc, aes(x = high_use, y = famrel))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("sex")

# initialize a plot of high_use and going out
g2 <- ggplot(alc, aes(x = high_use, y = goout))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ylab("going out")

# initialize a plot of high_use and absences
g3 <- ggplot(alc, aes(x = high_use, y = absences))

# define the plot as a boxplot and draw it
g3 + geom_boxplot() + ylab("absences")

# initialize a plot of high_use and G3
g4 <- ggplot(alc, aes(x = high_use, y = G3))

# define the plot as a boxplot and draw it
g4 + geom_boxplot() + ylab("grade")

# produce summary statistics by group
alc %>% group_by(high_use) %>% summarise(count = n(), famrel = mean(famrel), goout = mean(goout), absences = mean(absences), mean_grade = mean(G3))

# ---- 5 -----

# find the model with glm()
m <- glm(high_use ~ famrel + goout + absences + G3, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- exp(confint(m))

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

# ---- 6 -----

# find the model with glm()
m2 <- glm(high_use ~ famrel + goout + absences, data = alc, family = "binomial")

# print out a summary of the model
summary(m2)

# print out the coefficients of the model
coef(m2)

# predict() the probability of high_use
probabilities <- predict(m2, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, famrel, goout, absences, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(alc$high_use, alc$probability)

# ---- 7 -----

library(boot)

# K-fold cross-validation
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m2, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]

