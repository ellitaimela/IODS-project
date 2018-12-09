# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 5
#
# Part 1: Data Wrangling
#
# Elli Taimela
# 08.12.2018
# 


library(dplyr)
library(tidyr)
library(ggplot2)


BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", 
                    sep=" ", header=TRUE)

rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", 
                   sep="\t", header=TRUE)


# Look at the (column) names of BPRS
names(BPRS)
# Look at the structure of BPRS
str(BPRS)
# Print out summaries of the variables
summary(BPRS)

# Look at the (column) names of rats
names(rats)
# Look at the structure of rats
str(rats)
# Print out summaries of the variables
summary(rats)

# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

rats$ID <- factor(rats$ID)
rats$Group <- factor(rats$Group)

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks, 5, 5)))
# Take a glimpse at the BPRSL data
glimpse(BPRSL)

# Convert to long form
ratsl <-  rats %>% gather(key = WDs, value = weight, -ID, -Group)
# Extract the week number
ratsl <-  ratsl %>% mutate(WD = as.integer(substr(WDs, 3, 4)))
# Take a glimpse at the BPRSL data
glimpse(ratsl)

# Look at the (column) names of BPRS
names(BPRSL)
# Look at the structure of BPRS
str(BPRSL)
# Print out summaries of the variables
summary(BPRSL)

# Look at the (column) names of rats
names(ratsl)
# Look at the structure of rats
str(ratsl)
# Print out summaries of the variables
summary(ratsl)




