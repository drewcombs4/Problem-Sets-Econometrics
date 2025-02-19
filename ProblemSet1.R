###################################################
### Problem Set 1: Introduction to Econometrics ###
### Spring 2025                                 ###
### Author: Drew Combs                          ###
###################################################

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

#Mean Values for WaterSkiiers Ranked from Greatest to Least
LakeConroe_meanY <- mean(Waterskiers$costC) ##1
LakeSomerville_meanY <- mean(Waterskiers$costS) ##2
LakeHouston_meanY <- mean(Waterskiers$costH) ##3
rank(c(LakeConroe_meanY,LakeSomerville_meanY,LakeHouston_meanY))

#Mean Values for NonWaterSkiiers Ranked from Greatest to Least
LakeConroe_meanN <- mean(NonWaterskiers$costC) ##1
LakeSomerville_meanN <- mean(NonWaterskiers$costS) ##2
LakeHouston_meanN <- mean(NonWaterskiers$costH) ##3
rank(c(LakeConroe_meanN,LakeSomerville_meanN,LakeHouston_meanN))

#Median Values for WaterSkiiers Ranked from Greatest to Least
LakeConroe_medianY <- median(Waterskiers$costC) ##1
LakeSomerville_medianY <- median(Waterskiers$costS) ##2
LakeHouston_medianY <- median(Waterskiers$costH) ##3
rank(c(LakeConroe_medianY,LakeSomerville_medianY,LakeHouston_medianY))

#Median Values for NonWaterSkiiers Ranked from Greatest to Least
LakeConroe_medianN <- median(NonWaterskiers$costC) ##1
LakeSomerville_medianN <- median(NonWaterskiers$costS) ##2
LakeHouston_medianN <- median(NonWaterskiers$costH) ##3
rank(c(LakeConroe_medianN,LakeSomerville_medianN,LakeHouston_medianN))

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