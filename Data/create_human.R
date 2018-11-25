# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 4
#
# Part 2: Data Wrangling
#
# Elli Taimela
# 25.11.2018
# 


# Loading the data
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", 
               stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", 
                stringsAsFactors = F, na.strings = "..")

# Exploring the datasets: structure, dimensions and summaries
str(hd)
dim(hd)
summary(hd)

str(gii)
dim(gii)
summary(gii)

# Mutate gii and add new ratio variables 
gii <- mutate(gii, edu_ratio = Population.with.Secondary.Education..Female./Population.with.Secondary.Education..Male.)
gii <- mutate(gii, laborforce_ratio = Labour.Force.Participation.Rate..Female./Labour.Force.Participation.Rate..Male.)

# Join datasets
human <- inner_join(hd, gii, by = "Country", suffix = c(".hd", ".gii"))

# Explore the new dataset
str(human)
dim(human)
summary(human)
