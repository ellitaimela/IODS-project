# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 3
#
# Part 1: Analysis
#
# Elli Taimela
# 22.11.2018
# 
# This exercise analyses xxxxx. 
# 

# -------- 2 --------

# install.packages("tidyverse")
# install.packages("corrplot")

library(MASS)
library(GGally)
library(tidyverse)
library(corrplot)
library(dplyr)

data(Boston)

head(Boston)

str(Boston)
dim(Boston)

# -------- 3 --------

#pairs(Boston)
#ggpairs(Boston, lower = list(combo = wrap("facethist", bins = 20)))

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)

summary(Boston)

# -------- 4 --------

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)
#boston_scaled
# ggpairs(boston_scaled, lower = list(combo = wrap("facethist", bins = 20)))

boston_scaled$crim

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

label_vector <- c("low", "med_low", "med_high", "high")

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = label_vector) 
crime
# look at the table of the new factor crime
table(crime)
# remove original crim from the dataset
# boston_scaled <- dplyr::select(boston_scaled, -crime)
boston_scaled <- subset(boston_scaled, select = -c(crim))
# number of rows in the Boston dataset 
n <- nrow(boston_scaled)
n
# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]
crime
# save the correct classes from test data
correct_classes <- test$crime

test <- subset(test, select = -c(crime))


# -------- 5 --------


# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)
str(lda.pred)
# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train$crim)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen=2, col=classes, pch=classes)
lda.arrows(lda.fit, myscale = 2)

# -------- 6 --------

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# -------- 7 --------

# load MASS and Boston
library(MASS)
data('Boston')

boston_scaled.new <- scale(Boston)

# euclidean distance matrix
dist_eu <- dist(boston_scaled.new)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(boston_scaled.new, method="manhattan")

# look at the summary of the distances
summary(dist_man)

# k-means clustering
km <-kmeans(Boston, centers = 4)

# plot the Boston dataset with clusters
pairs(Boston[1:2], col = km$cluster)
# -------- 8 --------

# k-means clustering
km <-kmeans(Boston, centers = 4)

# plot the Boston dataset with clusters
pairs(Boston[1:2], col = km$cluster)

