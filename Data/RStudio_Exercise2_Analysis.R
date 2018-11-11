# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 2
#
# Part 2: Analysis
#
# Elli Taimela
# 10.11.2018
# 
# This exercise analyses students' learning data from the course Introduction to Social Statistics, 
# collected during 3.12.2014 - 10.1.2015. 
# 

install.packages("GGally")

library(dplyr)
library(ggplot2)
library(GGally)

# Reading the data
lrn14 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt", 
                    sep=",", header=TRUE)
# The data includes students' learning data from the course Introduction to Social Statistics, 
# collected during 3.12.2014 - 10.1.2015

# Exploring the structure and dimensions of the data; 166 observations, of 7 variables
str(lrn14)
dim(lrn14)

# Summaries of the variables
summary(lrn14)

ggpairs(lrn14, lower = list(combo = wrap("facethist", bins = 20)))

# Plotting the distributions of the variables
boxplot(lrn14)

# 110 females, 56 males
barplot(table(lrn14$gender))

# Age distribution from 17 to 55, median 22, mean 25.51, standard deviation 7.766078
hist(lrn14$age)
sd(lrn14$age)

# Attitude from 1 to 5, median 3.2, mean 3.14, standard deviation 0.5327656
hist(lrn14$attitude)
sd(lrn14$attitude)

# Points of deep learning questions from 1 to 5, median 3.667, mean 3.680, sd 0.5541369
hist(lrn14$deep)
sd(lrn14$deep)

# Points of strategic learning questions from 1 to 5, median 3.188, mean 3.121, sd 0.7718318
hist(lrn14$stra)
sd(lrn14$stra)

# Points of surface learning questions from 1 to 5, median 2.833, mean 2.787, sd 0.5288405
hist(lrn14$surf)
sd(lrn14$surf)

# Points of deep learning questions from 1 to 5, median 23.00, mean 22.72, sd 5.894884
hist(lrn14$points)
sd(lrn14$points)

# A strong positive colleration between attitude and points can be found
qplot(attitude, points, data = lrn14) + geom_smooth(method = "lm")
qplot(age, points, data = lrn14) + geom_smooth(method = "lm")
qplot(surf, points, data = lrn14) + geom_smooth(method = "lm")

# There is no correlation between age and attitude
qplot(age, attitude, data = lrn14) + geom_smooth(method = "lm")

# The correlation between age to points achieved in strategic and deep questions is not strong 
qplot(age, stra, data = lrn14) + geom_smooth(method = "lm")
qplot(age, deep, data = lrn14) + geom_smooth(method = "lm")
# There is negative correlation between age and points achieved in surface questions
qplot(age, surf, data = lrn14) + geom_smooth(method = "lm")

# On average, men achieved marginally higher points compared to women
plot(lrn14$points ~ lrn14$gender)

# On average, women achieved higher points in strategic and surface learning questions
plot(lrn14$stra ~ lrn14$gender)
plot(lrn14$deep ~ lrn14$gender)
plot(lrn14$surf ~ lrn14$gender)


# --------------------------------------- 3 --------------------------------------- 
# Fitting a linear model
# model1 <- lm(points ~ attitude + age + factor(gender), data = lrn14)
model1 <- lm(points ~ attitude + stra + surf, data = lrn14)
summary(model1)
# Of the explanatory variables, only attitude statistically significant, p-value of attitude 8.34e-09
# Combinations of all variables in the datasets were tested, no other statistically significant
# explanatory variable found than attitude. 

# Fitting a new model with attitude as the only explanatory variable
model2 <- lm(points ~ attitude, data = lrn14)
summary(model2)
# A statistically significant regression model found (p-value 4.119e-09). 


# --------------------------------------- 5 --------------------------------------- 

# Visualizing Residuals vs. Fitted values
plot(model2, which = 1)

# Visualizing a normal QQ-plot
plot(model2, which = 2)

# Visualizing Residuals vs. Leverage
plot(model2, which = 5)

