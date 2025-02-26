###################################################
### Midterm: Introduction to Econometrics       ###
### Spring 2025                                 ###
### Author: Drew Combs                          ###
###################################################
####### R-Session 2 ()
################
# INTRODUCTION #
#
# The hashtag (#) in R indicates a comment. Comments are lines of code that R will ignore when executing your code
#
# An R Script is a file where you can store commands that you will have R execute, it allows you to keep track of commands
# so you can re-run them and edit them where necessary
#
# You should be leaving comments throughout your R scripts so you can easily read and understand what the script is doing
# before you even run the script
#
##########################
# STARTING YOUR R SCRIPT #
#
rm(list = ls()) 
# ls(): is a command that lists all of the named variables currently in the R environment
# rm(): is a command that "removes" the variables that you provide in the list = command
#
getwd()
# getwd(): is a command that tells you which folder in your computer (your working directory, or wd) that
#          R is currently "looking" for files
#
cpath <-  "C:/Users/gretc/Documents/Temple/Teaching/Econometrics/2025/R_Sessions/Ch2_GoodDataPractices"
# assign a variable "cpath" using "<-" that tells R the folder where the files we will use for this exercise are located
# the above cpath is for my computer, you should update this for your own
#
setwd(cpath)
# setwd(): switches the folder that R will look for files in to the folder we set "cpath" to be
#
#
#####################
# WORKING WITH DATA #
#
# R is a coding language that allows you to store information as different types of objects
#  Examples of some object types:
#   scalar: a single value stored as a variable
#   vector: multiple values stores as a variable, vectors are indexed from 1 to the number of elements
#   matrix: store multiple values in rows and columns
#   data.frame: you can think of this as a table, including labels that contains data in each row, 
#               and the label for the data in the column names
#

#####################
# Create a scalar
x <- 8

#####################
# Create a vector
x1 <- c(8,9,10,11)

#display the first element of the vector
x1[1]

#add the first and last elements of the vector together and store as a new scalar
x2 <- x1[1] + x1[4]

#create a vector containing every integer from 10 to 17
x3 <- seq(10, 17)

#create a vector containing every value from 10 to 17 by adding 0.5 each time
x4 <- seq(10, 17, by=0.5)

#display the first 3 values of x4
x4[1:3]

#####################
# Create a matrix
a <- matrix(c(3, 4, 5, 6, 7, 8), nrow = 2, ncol= 3)

#display the dimensions (rows, columns)
dim(a)

#display first row of a
a[1,]

#display the second column of a
a[,2]

#create a matrix of all ones
b <- matrix(data=1, nrow=2, ncol=3)

#add b as new rows to a
c <- rbind(a,b)

#add b as new columns to a
d <- cbind(a,b)

#multiply the first element of c to the last element of d
c[1,1]*d[2,6]

#####################
# Create a dataframe
#R has some built-in datasets. Let's use state.x77

#to view the codebook for the dataset, use the command help
help(state.x77)

#you can also use the question mark to get help, and you can use it to learn more about how commands work
?mean

#take the state.x77 data and turn it into a dataframe
states_data <- data.frame(state.x77)

#we can display the column names for the dataframe
colnames(states_data)

#we can also display the row names for the dataframe (doesn't always exist)
rownames(states_data)

#we can also get a summary of some statistics by column
summary(states_data)

#to reference a specific column of data, we use the dollar sign
states_data$Population

#we can also calculate our own descriptive statistics outside of the summary command
mean(states_data$Population)
sd(states_data$Population)
min(states_data$Population)
max(states_data$Population)
length(states_data$Population)

#what if we just want to know the Virginia data? -- we can tell R to print the Virginia row
states_data[row.names(states_data)=="Virginia",] 
# Note the double equals tells R to check the condition is true, if we don't use this, then R thinks it should assign values, which we don't want
# row.names() returns the row names of the states_data dataframe, we need the comma to print out all of the column values for the row where the name is Virginia

#Now we can look at some visuals -- starting with histograms. Additional helpful commands: help(hist), colors()-- lists all the colors R knows
hist(states_data$Population)
hist(states_data$Income, xlab="Income", main=paste("Histogram of Income"), breaks = 20, col="cadetblue4")

#Let's take a look at scatter plots next
plot(states_data$Population, states_data$Murder)

## There are a ton of ways to make a figure nicer.  Here are a couple examples:
plot(states_data$Population, states_data$Murder, pch=2)

plot(states_data$Population, states_data$Murder, main="Murder and State Population", 
     pch=20, col="maroon", xlab="Population", ylab="Number of Murders")

#here's a list of all the colors R knows
colors()

#################################################################################
#############################
# EXERCISE: CLEANING DATA #
#
# You can also load pre-saved data into your R environment 
load("DonutX.RData")

#let's call this dataset something that makes sense and remove the old dta dataframe
donuts <- dta
rm(dta)

#What's wrong with the data? each variable has an error
View(donuts)

#create a scatter plot of the corrected data to look like the scatter plot we discussed in class

###### R-Session 3 (Bivariate OLS) ########
################
# INTRODUCTION #

# In this session we'll learn more about working with dataframes, how to perform bivariate regressions, and how to analyze results

##########################
# STARTING YOUR R SCRIPT #

#clear memory first

rm(list=ls())

#set working directory

cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"
setwd(cpath)

#install packages 
#a package contains a collection of functions, or sometimes datasets. If you want to use either the functions or the datasets
# in a package you have to first install the package, then tell R to use that package in this script with the "library" command
# you only have to install a package once

#install the "MASS" package
install.packages("MASS")

#include the packages we'll need to run this script
#MASS
#sandwich
#lmtest
install.packages("sandwich")
install.packages("lmtest")
library("lmtest")
library("sandwich")
##########################
#### Data Exploration ####
#pull in the height data
library("MASS")
load("Height_and_Wages_UK.RData")

#let's call this dataset something that makes sense and remove the old dta dataframe
height_wages <- dta 
rm(dta)

#we can save this dataset to a csv
write.csv(height_wages,"Height_and_Wages_UK.csv",row.names = FALSE)

#we can also reload the csv to a dataframe
height_wages <- read.csv("Height_and_Wages_UK.csv")

##################################
##### Understanding the Data #####

#Get summary statistics on the dataset
summary(height_wages)

#Plot the relationship between height and wages
plot(height_wages$height33,height_wages$gwage33, main= "Height and Wages UK Men at 33",
     pch=16, col= "lightsteelblue", xlab = "Height (inches)" , ylab= "Wages (GBP)")

#Calculate the covariance and correlation between height and wages?
cov(height_wages$height33,height_wages$gwage33)
cor(height_wages$height33,height_wages$gwage33)

#######################
### Subsetting Data ###

#For now, let's focus our analysis on men between 5'5" and 6'4", and men with positive wages less than 600
height_wages_subset <- height_wages[(height_wages$height33 >= 65 &
                                       height_wages$height33 <= 72 &
                                       height_wages$gwage33 >= 0 &
                                       height_wages$gwage33 <= 600),]

#let's look at a scatter plot of this relationship
plot(height_wages_subset$height33,height_wages_subset$gwage33, main= "Height and Wages UK Men at 33",
     pch=16, col= "lightsteelblue", xlab = "Height (inches)" , ylab= "Wages (GBP)")

#####################
#### Regressions ####

#regress wages on height
height_wages_results <- lm(height_wages_subset$gwage33~height_wages_subset$height33)
summary(height_wages_results)

#let's plot the data and the regression line
plot(height_wages_subset$height33, height_wages_subset$gwage33, main= "Height and Wages UK Men at 33",
     pch=16, col= "lightsteelblue", xlab = "Height (inches)" , ylab= "Wages (GBP)")
abline(lm(height_wages_subset$gwage33~height_wages_subset$height33), col= "black")

#what's the predicted wage for a man with a height of 67 inches?
print(height_wages_results$coefficients[1] + height_wages_results$coefficients[2]*67)

#what's the predicted wage for a man with a height of 73 inches?
print(height_wages_results$coefficients[1] + height_wages_results$coefficients[2]*73)

#########################################################
### Let's look at some heteroskedasticity adjustments ###

#re-run the original regression of wages on height
height_wages_results <- lm(gwage33~height33, data=height_wages_subset)
summary(height_wages_results)

#Let's adjust the standard errors for heteroskedasticity

height_wages_results_robust_se <- coeftest(height_wages_results, vcov= vcovHC, type= "HC1")
print(height_wages_results_robust_se)




########### R-Session 5 - Multivariate OLS #################
################
# INTRODUCTION #
#
# In this session we'll learn about regressions and testing in models with multiple independent variables
#
##########################
# STARTING YOUR R SCRIPT #
#
#clear memory first
rm(list=ls())

#set working directory
cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"
setwd(cpath)

#load packages
library(car)

##################################
#### Multivariate Regressions ####
#pull in the US height wages data

load("Height_and_Wages_US.RData")
#let's call this dataset something that makes sense and remove the old dta dataframe
height_wages_us <- dta
rm(dta)

####################################
#### Height and Wages in the US ####

#########
### Compare Bivariate and Multivariate Outputs ###
#first let's run a bivariate regression of wages against adult height
wage_adult_height_results <- lm(wage96 ~ height85, data = height_wages_us)
summary(wage_adult_height_results)

############
#Compare these results to what we get from a multivariate regression that includes adolescent height
wage_adult_height_teen_height_results <- lm(wage96 ~ height85 + height81, data = height_wages_us)
summary(wage_adult_height_teen_height_results)

#How should we interpret these coefficients? What changes when we add in teenage height? What effect do you think this captures?

#########
### Colinearity Issues ###
#first create a female variable in our dataset
height_wages_us$female <- 1-height_wages_us$male

#run the regression -- what does R tell you?
wage_height_gender_results <- lm(wage96 ~ height85 + male + female, data = height_wages_us)
summary(wage_height_gender_results)


#################################
#### Other Omitted Variables ####
#What else do you think is missing from our regression? Let's look at number of clubs and if students were athletes
wage_height_activities_results <- lm(wage96 ~ height85 + height81 + clubnum + athlets, data = height_wages_us)
summary(wage_height_activities_results)

###############################################################################
#################
### F-testing ###

#let's subset our data to just the observations where our variables of interest are not na
height_wages_us_subset <- height_wages_us[!is.na(height_wages_us$wage96) & !is.na(height_wages_us$height81) & !is.na(height_wages_us$height85) 
                                          & !is.na(height_wages_us$clubnum) & !is.na(height_wages_us$athlets),]

#run the regression for the model we think is true, the "unrestricted regression" 
unrestricted_regression <- lm(wage96 ~ height85 + height81 + clubnum + athlets, data = height_wages_us_subset)
summary(unrestricted_regression)

##########################################################
# Does height affect wages? --- what kind of test is this?

#H_0: beta_1 = beta_2 = 0
#H_A: beta_1 !=0, or, beta_2!=0, or beta_1 !=0 and beta_2 !=0


### Conduct the F-test ###

#1) Calculate the F-stat

### there's a convenient package in R that will do this for you ###
linearHypothesis(unrestricted_regression, c("height85=0", "height81=0"))

### we can also calculate this manually by running the restricted regression ###
restricted_regression <- lm(wage96 ~ clubnum + athlets, data = height_wages_us_subset)
summary(restricted_regression)

#pull terms together into an f-stat
f_stat <- ((summary(unrestricted_regression)$r.squared - summary(restricted_regression)$r.squared)/2) / ((1-summary(unrestricted_regression)$r.squared)/unrestricted_regression$df)

print(f_stat)


#2) Calculate the critical values
?qf

# 0.05 significance:
p_0.05_ftest <-1-.05

#notice that you can retrieve the degrees of freedom from a regression
qf(p_0.05_ftest, 2, unrestricted_regression$df)


######################################################################################
# Does teenage height affect wages more than adult height -- what kind of test is this?

#H_0: beta_1 = beta_2
#H_A: beta_1 < beta_2


#1) Calculate the F-stat

### there's a convenient package in R that will do this for you ###
linearHypothesis(unrestricted_regression, c("height85=height81"))

### we can also calculate this manually by running the restricted regression ###

#first we need to create a column that is adult + teenage height
height_wages_us_subset$height_sum <- height_wages_us_subset$height81 + height_wages_us_subset$height85

#now we can run the restricted regression
restricted_regression <- lm(wage96 ~ height_sum + clubnum + athlets, data = height_wages_us_subset)

#pull terms together into an f-stat
f_stat <- ((summary(unrestricted_regression)$r.squared - summary(restricted_regression)$r.squared)/1) / ((1-summary(unrestricted_regression)$r.squared)/unrestricted_regression$df)

print(f_stat)


#2) Calculate the critical values

# 0.05 significance:
p_0.05_ftest = 1-0.05

qf(p_0.05_ftest, 1, unrestricted_regression$df)


############ R-Session 6 Dummy Variables #################

################
# INTRODUCTION #
#
# In this session we'll learn about dummy variables, categorical variables, and interactions
#
##########################
# STARTING YOUR R SCRIPT #
#
#clear memory first
rm(list = ls()) 

#set working directory
cpath <-  "C:/Users/gretc/Documents/Temple/Teaching/Econometrics/2025/R_Sessions/Ch6_DummyVariables"
setwd(cpath)

#load packages
library(ggplot2) #this package also lets us make very nice looking graphs

#########################
#### Dummy Variables ####
#pull in the speeding tickets data
load("Speeding_Tickets.RData")

#let's call this dataset something that makes sense and remove the old dta dataframe
speeding_tickets <- dta
rm(dta)

####################################
#### Interaction Terms ####

##############
#First let's run a regression focusing on the effect of how many miles per hour over the speed limit you were on the amount of the fine
fine_speeding_results <- lm(Amount ~ MPHover, data=speeding_tickets)
summary(fine_speeding_results)

#Let's plot a scatter plot of this relationship and the predicted regression line -- note that we could have used the tools we developed before
fine_speeding_plot <- ggplot(speeding_tickets, aes(x = MPHover, y = Amount)) +
  geom_point() +  # Add points for scatter plot
  #add the regression line
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Fine by MPH Over Speedlimit",
       x = "MHP Over Speedlimit",
       y = "Fine Amount") +
  theme_minimal()  # Use a minimal theme for aesthetics

#display the plot
print(fine_speeding_plot)

# Save the plot
ggsave("fine_speeding_plot.pdf", plot = fine_speeding_plot, width = 8, height = 6)


################
#How does being out of state effect the fine amount, controlling for miles per hour above speedlimit
fine_speeding_outstate_results <- lm(Amount ~ MPHover + OutState, data=speeding_tickets)
summary(fine_speeding_outstate_results)

#set the instate slope and intercept
in_intercept <- fine_speeding_outstate_results$coefficients[1]
in_slope <- fine_speeding_outstate_results$coefficients[2]

#set the out of state slope and intercept
out_intercept <- fine_speeding_outstate_results$coefficients[1]+fine_speeding_outstate_results$coefficients[3]
out_slope <- fine_speeding_outstate_results$coefficients[2]

#Let's plot a scatter plot of this relationship and the predicted regression line -- note that we could have used the tools we developed before
fine_speeding_outstate_plot <- ggplot(speeding_tickets, aes(x = MPHover, y = Amount)) +
  geom_point() +  # Add points for scatter plot
  #add the regression lines
  geom_abline(intercept = in_intercept, slope = in_slope, size=1.1, color="darkgoldenrod")  + 
  geom_abline(intercept = out_intercept, slope = out_slope, size=1.1, color="cadetblue3")  + 
  labs(title = "Fine by MPH Over Speedlimit, In and Out of State Interact",
       x = "MHP Over Speedlimit",
       y = "Fine Amount") +
  theme_minimal()  # Use a minimal theme for aesthetics

#display the plot
print(fine_speeding_outstate_plot)

# Save the plot
ggsave("fine_speeding_outstate_plot.pdf", plot = fine_speeding_outstate_plot, width = 8, height = 6)


################
#How does the above regression compare to one where we add interaction terms? 

#first we need to create the interacted variable
speeding_tickets$MPH_OutState <- speeding_tickets$MPHover*speeding_tickets$OutState

fine_speeding_outstate_interact_results <- lm(Amount ~ MPHover + OutState + MPH_OutState, data=speeding_tickets)
summary(fine_speeding_outstate_interact_results)

#set the instate slope and intercept
in_intercept <- fine_speeding_outstate_interact_results$coefficients[1]
in_slope <- fine_speeding_outstate_interact_results$coefficients[2]

#set the out of state slope and intercept
out_intercept <- fine_speeding_outstate_interact_results$coefficients[1]+fine_speeding_outstate_interact_results$coefficients[3]
out_slope <- fine_speeding_outstate_interact_results$coefficients[2]+fine_speeding_outstate_interact_results$coefficients[4]

#Let's plot a scatter plot of this relationship and the predicted regression line -- note that we could have used the tools we developed before
fine_speeding_plot_outstate_interact <- ggplot(speeding_tickets, aes(x = MPHover, y = Amount)) +
  geom_point() +  # Add points for scatter plot
  #add the regression lines
  geom_abline(intercept = in_intercept, slope = in_slope, size=1.1, color="darkgoldenrod")  + 
  geom_abline(intercept = out_intercept, slope = out_slope, size=1.1, color="cadetblue3")  + 
  labs(title = "Fine by MPH Over Speedlimit, In and Out of State Interact",
       x = "MHP Over Speedlimit",
       y = "Fine Amount") +
  theme_minimal()  # Use a minimal theme for aesthetics

#display the plot
print(fine_speeding_plot_outstate_interact)

# Save the plot
ggsave("fine_speeding_plot_outstate_interact.pdf", plot = fine_speeding_plot_outstate_interact, width = 8, height = 6)


######################################################################################################################
##############
### Extras ###

################
#How is this different from running two separate regressions?
in_state_data <- speeding_tickets[speeding_tickets$OutState==0,]
out_state_data <- speeding_tickets[speeding_tickets$OutState==1,]

#########
#instate
in_state_fine_speeding_results <- lm(Amount ~ MPHover, data=in_state_data)
summary(in_state_fine_speeding_results)

#Let's plot a scatter plot of this relationship and the predicted regression line
fine_speeding_plot_instate_only <- ggplot(in_state_data, aes(x = MPHover, y = Amount)) +
  geom_point() +  # Add points for scatter plot
  #add the regression line
  geom_smooth(method = "lm", se = FALSE, color="darkgoldenrod") +  
  labs(title = "Instate Fine by MPH Over Speedlimit",
       x = "MHP Over Speedlimit",
       y = "Fine Amount") +
  theme_minimal()  # Use a minimal theme for aesthetics

#display the plot
print(fine_speeding_plot_instate_only)

# Save the plot
ggsave("fine_speeding_plot_instate_only.pdf", plot = fine_speeding_plot_instate_only, width = 8, height = 6)


#########
#out of state
out_state_fine_speeding_results <- lm(Amount ~ MPHover, data=out_state_data)
summary(out_state_fine_speeding_results)

#Let's plot a scatter plot of this relationship and the predicted regression line
fine_speeding_plot_outstate_only <- ggplot(out_state_data, aes(x = MPHover, y = Amount)) +
  geom_point() +  # Add points for scatter plot
  #add the regression line
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Out of State Fine by MPH Over Speedlimit",
       x = "MHP Over Speedlimit",
       y = "Fine Amount") +
  theme_minimal()  # Use a minimal theme for aesthetics

#display the plot
print(fine_speeding_plot_outstate_only)

# Save the plot
ggsave("fine_speeding_plot_outstate_only.pdf", plot = fine_speeding_plot_outstate_only, width = 8, height = 6)



### Problem Set 1: Introduction to Econometrics ###


#clear memory first
rm(list=ls())
#set working directory
cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"
setwd(cpath)
#load libraries
#install.packages("AER")
library("AER")
#read in the dataset 
data("RecreationDemand" , package ="AER")

###############################
# Question 2a.: Explain what this data is about. How many observations are there? What is the unit of observation in this dataset? What are the names of the variables in this dataset?


#Number of Observations
n_obs <- nrow(RecreationDemand)
print(paste("Number of Observations:", n_obs))

#Units of Obvseration
help("RecreationDemand")
print(paste("Units of Obvservation: Individuals"))

#Variable Names
print(colnames(RecreationDemand))

###############################
# Question 2b.: Compute summary statistics for water skiers and non-water skiers separately. At which lake do skiers and non-skiers spend the most money based on the mean values? What about the median values? 

#Separates water skiiers and non water skiiers

"yes" <- (RecreationDemand$ski == 2)
Waterskiers <- subset(RecreationDemand, RecreationDemand$ski == "yes", )

"no" <- (RecreationDemand$ski == 1)
NonWaterskiers <- subset(RecreationDemand, RecreationDemand$ski == "no",)

#Summary Statistics for Waterskiers
summary(Waterskiers)

#Summary Statistics for Non-Waterskiers
summary(NonWaterskiers)


###############################
# Question 2c.: Generate histograms of the expenditures for skiers and non-skiers at Lake Somerville. Remember to include a label for the x-axis and a title. Is the data skewed? If so, in what direction? 

#Histogram of Skiers 
hist(Waterskiers$costS, ylab = "Number of Waterskiers", xlab="Expenditures in USD", main=paste("Histogram of Waterskier Expenditures when Visiting Lake Somerville"), breaks = 20, col="cadetblue4")

#Histogram of Non-Skiers
hist(NonWaterskiers$costS, ylab = "Number of Waterskiers", xlab="Expenditures in USD", main=paste("Histogram of Non Waterskier Expenditures when Visiting Lake Somerville"), breaks = 20, col="red")

#Data Skewness
install.packages("e1071")
library("e1071")

#Skewness for Non Waterskiers
Nonwaterskierskewness_value <- skewness(NonWaterskiers$costS)
if (Nonwaterskierskewness_value > 0) {
  print("Data is right-skewed")
} else if (Nonwaterskierskewness_value < 0) {
  print("Data is left-skewed")
} else {
  print("Data is approximately symmetric")
}

#Skewness for Waterskiers
waterskierskewness_value <- skewness(Waterskiers$costS)
if (waterskierskewness_value > 0) {
  print("Data is right-skewed")
} else if (waterskierskewness_value < 0) {
  print("Data is left-skewed")
} else {
  print("Data is approximately symmetric")
}
###############################
# Question 2d.: Examine the income variable. What units is this variable in? How can we change this to be in USD? Create a new variable called income_usd with the transformed data.

#Income Variable
help("RecreationDemand")
print(paste("Units of Income: Annual household income of the respondent (in 1,000 USD)"))

#Changing Income from 1000 USD to just USD

income_usd <- (RecreationDemand$income *1000)

waterskiersincome_usd <- (Waterskiers$income *1000)

nonskiersincome_usd <-(NonWaterskiers$income *1000)



###############################
# Question 2e.: For skiers and non-skiers separately, compute the covariance and correlation between income_usd and Lake Somerville expenditures. If we calculate these values using the income variable instead, what happens to these values? Why? 

#Waterskiers Covariance and Correlation
cov(waterskiersincome_usd, Waterskiers$costS)
cor(waterskiersincome_usd, Waterskiers$costS)

#Waterskiers Covariance and Correlation without USD converted
cov(Waterskiers$income, Waterskiers$costS)
cor(Waterskiers$income, Waterskiers$costS)

#NonWaterskiers Covariance and Correlation
cov(nonskiersincome_usd, NonWaterskiers$costS)
cor(nonskiersincome_usd, NonWaterskiers$costS)

#Waterskiers Covariance without USD converted
cov(NonWaterskiers$income, NonWaterskiers$costS)
cor(NonWaterskiers$income, NonWaterskiers$costS)

print("When the Income variable is adjusted correctly to Income_USD, the Income_USD variable has a covariance that is 1000x bigger than without adjustment (The original Income variable). The correlations stay the same even without the variable adjustments. This occurs because corrleation is scale-invariant")

###############################
# Question 2f.: Now calculate the covariances and correlations between expenditures at Lake Conroe and expenditures at Lake Somerville for skiers and non-skiers. Are these variables more or less correlated than income and expenditures? Why do you think this is the case? 

#Waterskiers Covariance and Correlation
cov(Waterskiers$costC, Waterskiers$costS)
cor(Waterskiers$costC, Waterskiers$costS)

#NonWaterskiers Covariance and Correlation
cov(NonWaterskiers$costC,NonWaterskiers$costS)
cor(NonWaterskiers$costC,NonWaterskiers$costS)

#Differences in correlations

DifferenceincorrelationbetweenNonskiers_Exp_and_income <- cor(NonWaterskiers$costC,NonWaterskiers$costS) - cor(nonskiersincome_usd, NonWaterskiers$costS)
Differenceincorrelationbetweenskiers_Exp_and_income <- cor(Waterskiers$costC, Waterskiers$costS) - cor(waterskiersincome_usd, Waterskiers$costS)

print(DifferenceincorrelationbetweenNonskiers_Exp_and_income)
print(Differenceincorrelationbetweenskiers_Exp_and_income)

#Why might they be different in correlation

print("The two expenditures have a greater correlation because they are shaped by similar factors (aka. preferences, lifestyle choices or needs). Since both lakes are in similar areas, only divided by 70 miles, their distance could explain why each has an effect on each other and how there may be a similar demographic attending each lake. On the contrary, income is a much broader measurement. Different people have different spending and saving habits. Similarities in income do not correlate with equal habits or preferences. Two people could make the same income, but spend it two totally different ways.")
###############################
# Question 2g.: Generate separate scatterplots of expenditures at Lake Somerville (y-axis) against expenditures at Lake Conroe (x-axis) for skiers and non-skiers. Remember to include a label for the x-axis, a label for the y-axis, and a title. 

#Scatter Plot for Non-Skiers
plot(NonWaterskiers$costC, NonWaterskiers$costS, main= "Expenditures at Lake Somerville and Lake Conroe for Non-Skiers",
     pch=16, col= "blue", xlab = "Expenditures Lake Conroe (USD)" , ylab= "Expenditures Lake Somerville (USD)")
abline(lm(NonWaterskiers$costS~NonWaterskiers$costC), col= "black")

#Scatter Plot for Non-Skiers
plot(Waterskiers$costC, Waterskiers$costS, main= "Expenditures at Lake Somerville and Lake Conroe for Skiers",
     pch=16, col= "green", xlab = "Expenditures Lake Conroe (USD)" , ylab= "Expenditures Lake Somerville (USD)")
abline(lm(NonWaterskiers$costS~NonWaterskiers$costC), col= "black")

###############################
# Question 2h.: Regress expenditures at Lake Somerville on expenditures at Lake Conroe for non-skiers. Repeat the analysis for skiers.

#Linear Regression for Non-Skiers
Nonskiers_model <- lm(formula = NonWaterskiers$costS~NonWaterskiers$costC, data = NonWaterskiers)
summary(Nonskiers_model)

#Linear Regression for skiers
Skiers_model <- lm(formula = Waterskiers$costS~Waterskiers$costC, data = NonWaterskiers)
summary(Skiers_model)


###############################
# Question 2i.: Interpret your regression results for the separate regressions. What does $\beta_0$ mean? What does $\beta_1$ mean?

#Coefficents for NonSkiers Model
Nonskiers_beta_0 <- (Nonskiers_model$coefficients[1])
NonSkiers_beta_1 <- (Nonskiers_model$coefficients[2])
print((Nonskiers_model$coefficients))


#Beta_0_ and Beta_1_ Meaning for Non-Skiers

print("When the x (x1) term is 0, Beta_0 is the predicted value of Y. In this particular regression, when the expeniditures are at $0 for Non-skiers at Lake Conroe, the expenditures at Lake Somerville total $5.72. Beta_1 represents the change in Y for a one-unit change in X. For every $1 increase in the expenditures at Lake Conroe, the expenditures at Lake Somerville from non-skiers increase by $.98.")

#Coefficents for NonSkiers Model
Skiers_beta_0 <- (Skiers_model$coefficients[1])
Skiers_beta_1 <- (Skiers_model$coefficients[2])
print((Skiers_model$coefficients))

#Beta_0_ and Beta_1_ Meaning for Skiers
print("When the x (x1) term is 0, Beta_0 is the predicted value of Y. In this particular regression, when the expeniditures are at $0 for Skiers at Lake Conroe, the expenditures at Lake Somerville total $6.84. Beta_1 represents the change in Y for a one-unit change in X. For every $1 increase in the expenditures at Lake Conroe, the expenditures at Lake Somerville from skiers increase by $.96.")
###############################

# Question 2j.: List at least three potential threats to exogeneity in these regressions.

print("Omitted Variable Bias, Reverse Causality, and Measurement Error")

#Omitted Variable Bias
print("When important factors impact the both independent variable (Lake Conroe's expenditures) and the dependent variable (Lake Somerville's expenditures) that were unaccounted for in the model. For example, visitor count at both lakes, which is not mentioned in the model.")

#Reverse Causality
print("Expenditures may be affected by factors that are manipulated by the physical characteristics of the lake. For instance, if Lake Somerville has an increase of tourism due to the lake's water quality growing. It might bring tourists to Lake Conroe, where its expenditures grow because of Lake Somerville (making Lake Somerville, no longer dependent on Lake Conroe.")

#Measurement Error
print("Expenditures may become biased estimates if measured incorrectly. Say the calculated expenditures were accidently totaled for the wrong year. This would cause a biased estimate for the expenditures as the modelis using incorrect data.")






##### Ps2 ########

#clear memory first
rm(list=ls())
#set working directory
cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"
setwd(cpath)
#load libraries
install.packages("Wooldridge")
library("wooldridge")
#load in the prison dataset 
data("prison" , package ="wooldridge")

## Using the “wooldridge” library, load the dataset “prison” into R and answer the following questions. 

## a. Explain what this data is about. How many observations are there? What is the unit of observation in this dataset? What are the names of the variables in this dataset (provide the command only)? Remember to include the commands required to answer this question.

#Explain what this data is about

help("prison")
print(paste("The data is about The Effect of Prison Population Size on Crime Rates"))

#Number of Observations
n_obs <- nrow(prison)
print(paste("Number of Observations:", n_obs))

#Units of Observation
help("prison")
print(paste("Units of Obvservation: States over Time"))

#Variable Names
print(colnames(prison))

## b. Compute the mean violent crime rate and the mean property crime rate. Generate a scatter plot of the relationship between property crime rates and violent crime rates. Remember to include axis labels and a title. Show your results.

summary(prison$crip)

summary(prison$criv)

mean(prison$crip)

mean(prison$criv)

plot(prison$crip, prison$criv, main= "Relationship Between Property Crime Rates and Violent Crime Rates",
     pch=16, col= "blue", xlab = "Property Crime Rates (per 100,000)" , ylab= "Violent Crime Rates (per 100,000)")

## c. Assume that you are using a bivariate model:

##violent crime ratei = β0 + β1 property crime ratei + εi

##H_0: beta_1 = 0
#H_A: beta_1 != 0

##d. Regress the violent crime rate on the property crime rate. Show and interpret your regression results. What does ˆβ0 mean? What does ˆβ1 mean?
Bivariate_model <- lm(formula =prison$criv~prison$crip, data = prison)
summary(Bivariate_model)

print("When the x (x1) term is 0, Beta_0 is the predicted value of Y. In this particular regression, when the property crime rate for a state is at 0, the violent crime rate for a state is -3.917401. Beta_1 represents the change in Y for a one-unit change in X. For every 1 increase in the units of poperty crime rate for a state, the violent crime rate in a state increases by $.193732.")

##e. Re-generate a scatter plot of the relationship between property crime rates and violent crime rates but include the regression line. Does your model fit the data well? Also assess the two other measures of goodness of fit that we discussed in class.
plot(prison$crip, prison$criv, main= "Relationship Between Property Crime Rates and Violent Crime Rates",
     pch=16, col= "red", xlab = "Property Crime Rates (per 100,000)" , ylab= "Violent Crime Rates (per 100,000)")
abline(lm(prison$criv~prison$crip), col= "black")

cov(prison$crip,prison$criv)
cor(prison$crip,prison$criv)

##f. What is the t-statistic for the property crime rate? How about the p-value? Calculate the critical values for significance levels of 0.1, 0.05, and 0.01. What do your results tell you about your hypotheses in part b?

#T-statistic
print(paste("T-statistic: 24.91"))

#P-Value
print(paste("P-Value: <2e-16"))
#critical values
#.01
p_001_two <- 1 - 0.01/2
df_violentcrimerate <- nrow(prison[!is.na(prison$criv) & !is.na(prison$crip),]) -2
df_violentcrimerate_results <- Bivariate_model$df
qt(p_001_two,df_violentcrimerate)

#0.05
p_005_two <- 1-0.05/2
qt(p_005_two,df_violentcrimerate)

#0.1
p_01_two <- 1-0.1/2
qt(p_01_two,df_violentcrimerate)

#Hypothesis:

print("The results above allow us to reject the null hypothesis that beta_1 = 0 or that there is no effect between property crime rates and violent crime rates among states. The t-statistic is much greater than 0 (24.91) meaning that it is extremely likely that there is a causation between criv and crip. Secondly, we also proved that the p-value is <2e-16 (extremely small and close to 0), showing that there is an extremely small probability that there is a Type I error. Since the p-value is below .05,.1, and .01, our hypothesis is statistically significant.")
##g. Explain the difference between homoscedasticity and heteroscedasticity. Do you think we have a heteroscedasticity problem here? Explain your reasoning. Recalculate the t-statistic for the property crime rate correcting for this potential heteroscedasticity problem. Do your estimates change a lot? Do your conclusions? Explain your results.

# Get the squared residuals
residuals_squared <- resid(Bivariate_model)^2
# Regress squared residuals on the property crime rate
bp_model <- lm(residuals_squared ~ prison$crip + fitted(Bivariate_model), data = prison)
# Breusch-Pagan test
bptest_result <- bptest(bp_model) #I don't remember how we did this in class, I used R-Bloggers to come up with this 
print(bptest_result)
# Robust Standard Errors 
robust_se <- coeftest(Bivariate_model, vcov. = vcovHC(Bivariate_model, type = "HC1"))
# Display original coefficients
print("Original Coefficients:")
print(summary(Bivariate_model)$coefficients)
# Display robust coefficients
print("Robust Coefficients:")
print(robust_se)
print("Homescedasticity refers to when the spread of the variance of the data points remains consistent regardless of the value of the independent variable. Heteroscedasticity refers to when there are trends between individuals within the error term, meaning different individuals' error terms are correlated; this leads to variances that shift depending on the value of the independent variable. I do believe we may have a heteroscedasticity problem as the graph of the residuals appears to show variances which increase as the value of the independent variable changes. Our estimates and conclusions do not change significantly as the problem with heteroscedasticity was not large, there was however a slight change so it is important that we corrected for this issue Changes: prision$crip original = .0078 robust = .0148 & Intercept original = .3732 robust = .6296 .")


##h. As an econometrician, do you think that there is a causal relationship here? Are there any issues with the conclusions you came to in parts f. and g.? Explain your reasoning.

print("As an econometrician I do not believe there is a casual relationship, while we observe a statistically signifigant association correlation does not equal causation. Factors such as ommited variables and simultaneity prevent us from being able to establish causality")

##i. List at least two variables that are potential threats to exogeneity in these regressions and explain why they might be causing problems.

print("Metro could threaten exogeneity because urban areas may have different property and violent crime opportunities and different police strategies. Unemployment may be another threat because higher unemployment rates may lead to more crime and fewer businesses in the area leading to higher levels of unemployment")  
