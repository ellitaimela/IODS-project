# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 2
#
# Part 1: Data Wrangling
#
# Elli Taimela
# 10.11.2018
# 

# install.packages("dplyr")
library(dplyr)

# Reading the learning data
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", 
                    sep="\t", header=TRUE)

# Checking the data, and the structure, dimensions and summary of the data
lrn14
str(lrn14)
dim(lrn14)
summary(lrn14)

# Defining questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", 
                    "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05", 
                       "SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# Selecting the columns related to deep learning and creating the column 'deep'
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$Deep <- rowMeans(deep_columns)

# Selecting the columns related to surface learning and creating the column 'surf'
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$Surf <- rowMeans(surface_columns)

# Selecting the columns related to strategic learning and creating the column 'stra'
strategic_columns <- select(lrn14, one_of(surface_questions))
lrn14$Stra <- rowMeans(strategic_columns)

# Selecting relevant columns and creating the new data set
columns_to_keep <- c("gender", "Age", "Attitude", "Deep", "Stra", "Surf", "Points")
learning2014 <- select(lrn14, one_of(columns_to_keep))
learning2014 <- filter(learning2014, Points>0)

# Checking the structure and dimensions of the filtered dataset: 166 obs, 7 var
dim(learning2014)
str(learning2014)

# Setting the working directory
setwd("/Users/ellitaimela/Documents/Github/IODS-project/Data")

# Writing a csv file of the dataset
write.csv(learning2014, file = "learning2014.csv")

# Reading the csv file
dataset <- read.csv("learning2014.csv", sep=",")

# Exploring the structure of the dataset in the csv file
str(dataset)
head(dataset)
summary(dataset)
