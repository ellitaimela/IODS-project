# Introduction to Open Data Science
# University of Helsinki
# RStudio Exercise 5
#
# Part 2: Analysis
#
# Elli Taimela
# 08.12.2018
# 
# This exercise analyses XXX
# 

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", 
                   sep=" ", header=TRUE)

rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", 
                   sep="\t", header=TRUE)

orig_data_BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", 
                   sep=" ", header=TRUE)

orig_data_rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", 
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
# Extract the wd
ratsl <-  ratsl %>% mutate(WD = as.integer(substr(WDs, 3, 4)))
# Take a glimpse at the BPRSL data
glimpse(ratsl)

# summary, structure, column names
names(BPRSL)
str(BPRSL)
summary(BPRSL)

# summary, structure, column names
names(ratsl)
str(ratsl)
summary(ratsl)


# --------------------------------------------------------------------------------------------------------

# Part I: 

# Introduction
ratsl

# Graphical displays of longitudinal data

# BPRS plots by groups
ggplot(ratsl, aes(x = WD, y = bprs, linetype = ID)) + geom_line() +
  scale_linetype_manual(values = rep(1:10, times = 4)) + facet_grid(.~Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(ratsl$bprs), max(ratsl$bprs)))


# Standardized BPRS plots by groups
ratsl <- ratsl %>% group_by(Group) %>% mutate(bprs_standardized = (bprs - mean(bprs))/sd(bprs)) %>% ungroup()

ggplot(ratsl, aes(x = WD, y = bprs_standardized, linetype = ID)) + geom_line() +
  scale_linetype_manual(values = rep(1:10, times = 4)) + facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")

  
# Summary measure analysis of longitudinal data
n <- ratsl$WD %>% unique() %>% length()
ratss <- ratsl %>% group_by(Group, WD) %>% summarise( mean = mean(bprs), se = sd(bprs)/sqrt(n)) %>% ungroup()

glimpse(ratss)

# Plot the mean profiles
ggplot(ratss, aes(x = WD, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")

ggplot(ratss, aes(x = WD, y = mean, linetype = Group, shape = Group)) + geom_line() +
  scale_linetype_manual(values = c(1,2,3)) + geom_point(size=3) + scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) + theme(legend.position = "top") +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

# Create a summary data by treatment and subject with mean as the summary variable.
ratsl8s <- ratsl %>%
  filter(WD > 1) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(bprs)) %>%
  ungroup()

# Glimpse the data
glimpse(ratsl8s)

# Draw a boxplot of the mean versus group
ggplot(ratsl8s, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight), time 0-60")

# one method to remove the outliers
ratsl8s_1 <- ratsl8s %>% filter(Group == 3)
ratsl8s_1 <- ratsl8s_1 %>% filter(mean > 500)
ratsl8s_2 <- ratsl8s %>% filter(Group != 3)
ratsl8s_2 <- ratsl8s_2 %>% filter(mean>255) %>% filter(mean < 550)
ratsl8s <- rbind(ratsl8s_2, ratsl8s_1)

ggplot(ratsl8s, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(weight), whole study period")


# Fit the linear model with the mean as the response 
fit <- lm(mean ~ Group, data=ratsl8s)
anova(fit)



# additional shit


# esimerkki
# Convert data to long form
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD, 3, 4))) 





# --------------------------------------------------------------------------------------------------------

# Part II

BPRSL$treatment <- factor(BPRSL$treatment)
BPRSL$subject <- factor(BPRSL$subject)
glimpse(BPRSL)

# Check the dimensions of the data
dim(BPRSL)

# Plot of symptoms against time, ignoring the repeated-measures structure of the data 
# but identifying the group to which each observation belongs.

ggplot(BPRSL, aes(x = week, y = bprs)) + geom_text(aes(linetype = treatment, label = treatment)) 
+ scale_x_continuous(name = "Time (days)") + scale_y_continuous(name = "Weight (grams)") 
+ theme(legend.position = "top")








# Plot the RATSL data
ggplot(BPRSL, aes(x = week, y = bprs, group = treatment)) +
  geom_line()

# dplyr, tidyr, RATS and RATSL are available

# create a regression model RATS_reg
RATS_reg <- "Regression model here!"

# print out a summary of the model
# dplyr, tidyr, RATS and RATSL are available

# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)

# Print the summary of the model

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model


# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)

# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model with the interaction
RATS_ref2 <- "Write the model here"

# print a summary of the model


# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# draw the plot of RATSL with the observed Weight values
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- "Change me!"

# Create a new column fitted to RATSL


# draw the plot of RATSL with the Fitted values of weight
ggplot(RATSL, aes(x = Time, y = "Change me!", group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")







# Linear regression: ignoring the lognitudinal nature of the data

# create a regression model BPRS_reg
BPRS_reg <- lm(data = BPRSL, bprs ~ week + treatment + week*treatment)

# print out a summary of the model
summary(BPRS_reg)

# Draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

# Create a random intercept model
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL, REML = FALSE)

summary(BPRS_ref)

# create a random intercept and random slope model
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref1)

# perform an ANOVA test on the two models
anova(BPRS_ref1, BPRS_ref)

# create a random intercept and random slope model with the interaction
BPRS_ref2 <- lmer(bprs ~ week + treatment + week * treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref2)

# perform an ANOVA test on the two models
anova(BPRS_ref2, BPRS_ref1)

# Create a vector of the fitted values
Fitted <- fitted(BPRS_ref1)

# Create a new column fitted to bprSL
mutate(BPRSL, Fitted = Fitted)

# draw the plot of BPRSL with the Fitted values of symptoms
ggplot(BPRSL, aes(x = week, y = Fitted, group = treatment)) +
  geom_smooth(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)") +
  scale_y_continuous(name = "Fitted bprs (score)") +
  theme(legend.position = "top")


