###################################################
### Problem Set 1: Introduction to Econometrics ###
### Spring 2025                                 ###
### Author: Drew Combs, Declan Woods and Ben A. ###
###################################################

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

plot(prison$criv, prison$crip, main= "Relationship Between Property Crime Rates and Violent Crime Rates",
     pch=16, col= "blue", xlab = "Property Crime Rates (per 100,000)" , ylab= "Violent Crime Rates (per 100,000)")

## c. Assume that you are using a bivariate model:

  ##violent crime ratei = β0 + β1 property crime ratei + εi

##d. Regress the violent crime rate on the property crime rate. Show and interpret your regression results. What does ˆβ0 mean? What does ˆβ1 mean?
Bivariate_model <- lm(formula =prison$crip~prison$criv, data = prison)
summary(Bivariate_model)

print("When the x (x1) term is 0, Beta_0 is the predicted value of Y. In this particular regression, when the property crime rate for a state is at 0, the violent crime rate for a state is -3.917401. Beta_1 represents the change in Y for a one-unit change in X. For every 1 increase in the units of poperty crime rate for a state, the violent crime rate in a state increases by $.193732.")

##e. Re-generate a scatter plot of the relationship between property crime rates and violent crime rates but include the regression line. Does your model fit the data well? Also assess the two other measures of goodness of fit that we discussed in class.
plot(prison$criv, prison$crip, main= "Relationship Between Property Crime Rates and Violent Crime Rates",
     pch=16, col= "red", xlab = "Property Crime Rates (per 100,000)" , ylab= "Violent Crime Rates (per 100,000)")
abline(lm(prison$crip~prison$criv), col= "black")

cov(prison$criv,prison$crip)
cor(prison$criv,prison$crip)
      
##f. What is the t-statistic for the property crime rate? How about the p-value? Calculate the critical values for significance levels of 0.1, 0.05, and 0.01. What do your results tell you about your hypotheses in part b?

#T-statistic
print(paste("T-statistic: 24.91"))

#P-Value
print(paste("P-Value: <2e-16"))
#critical values
#.01
p_001_two <- 1 - 0.01/2
df_violentcrimerate <- nrow(prison[!is.na(prison$criv) & !is.na(prison$crip),]) -2
df_violentcrimerate_results <- violentcrimerate_results$df
qt(p_001_two,df_violentcrimerate)

#0.05
p_005_two <- 1-0.05/2
qt(p_005_two,df_violentcrimerate)

#0.1
p_01_two <- 1-0.1/2
qt(p_01_two,df_violentcrimerate)

##g. Explain the difference between homoscedasticity and heteroscedasticity. Do you think we have a heteroscedasticity problem here? Explain your reasoning. Recalculate the t-statistic for the property crime rate correcting for this potential heteroscedasticity problem. Do your estimates change a lot? Do your conclusions? Explain your results.

      
##h. As an econometrician, do you think that there is a causal relationship here? Are there any issues with the conclusions you came to in parts f. and g.? Explain your reasoning.

      
##i. List at least two variables that are potential threats to exogeneity in these regressions and explain why they might be causing problems.

      
