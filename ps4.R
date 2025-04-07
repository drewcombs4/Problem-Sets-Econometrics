###################################################
### Problem Set 4                               ###
### Spring 2025                                 ###
### Author: Drew Combs, Ben Abernathy           ###
###################################################
####### 
################
# INTRODUCTION #

#clear memory first
rm(list=ls())

#set working directory
cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"
setwd(cpath)

#Using the “Ecdat” library, load the dataset “Crime” into R and answer the following questions.

install.packages("Ecdat")
library(Ecdat)
install.packages("ggplot2")
library(ggplot2)
data("Crime" , package ="Ecdat")

#a. What are the units of observation for this dataset?
str(Crime)
print(paste("Units of Observation: Counties in North Carolina between 1981-1987 "))

#Generate a scatterplot of crime rate (on the y-axis) against the police per capita,polpc, (on the x-axis). 
Crime_Rate_x_POLPC_Plot <- ggplot(Crime, aes(x = polpc, y = crmrte)) +
  geom_point(aes(color = factor(county)), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Crime Rate vs Police Per Capita",
    x = "Police Per Capita",
    y = "Crime Rate",
    color = "County"
  ) +
  theme_minimal()

#display the plot
print(Crime_Rate_x_POLPC_Plot)

# Save the plot
ggsave("Crime_Rate_x_POLPC_Plot.pdf", plot = Crime_Rate_x_POLPC_Plot, width = 8, height = 6)


#Generate another scatterplot of crime rate (on the y-axis) against the ratio of arrests to reported crimes, prbarr, (on the x-axis). Remember to include axis labels and a title. 
Crime_Rate_x_PRBARR_Plot <- ggplot(Crime, aes(x = prbarr, y = crmrte)) +
  geom_point(aes(color = factor(county)), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
labs(title = "Crime Rate vs Probability of Arrest",
     x = "Probability of Arrest",
     y = "Crime Rate",
     color = "County"
) +
  theme_minimal()

#display the plot
print(Crime_Rate_x_PRBARR_Plot)

# Save the plot
ggsave("Crime_Rate_x_PRBARR_Plott.pdf", plot = Crime_Rate_x_PRBARR_Plot, width = 8, height = 6)

#Do you agree that this variable should be called “probability of arrest”? 
#What about calling this a probability seems odd?
print("No, it does not make sense to call this a probability of arrest because this is simply a ratio of arrests to reported crimes, which is not a probability. This becomes obvious when viewing the results and seeing that the range for this variable extends well beyond 0 to 1 (the range for a probability, with 0 being that a said event will certainly not happen and 1 being that a said event will certainly happen) because the variables range extends beyond this range, it cannot be a probability.")

#b. Assume that you are using a multivariate model: crmrteit = β0 + β1polpcit + β2prbarrit + β3prbconvit + β4avgsenit + εi

#Run this regression then show and interpret your regression results. 

Crime_Rate_Results <- lm(Crime$crmrte ~ Crime$polpc + Crime$prbarr + Crime$prbconv + Crime$avgsen , data = Crime)

summary(Crime_Rate_Results)

#What are β0, ˆβ1, ˆβ2, ˆβ3, ˆβ4?
#What do they mean?

print("All are estimates for the true values of the coefficients.
B^0: Baseline crime rate when all explanatory variables are 0.
B^1: A one-unit increase in police per capita is associated with an increase of 3.06 in the crime rate, holding other variables constant. 
B^2: A one-unit increase in the probability of arrest is associated with a decrease of 0.051 in crime rate.
B^3: A one-unit increase in the probability of conviction leads to a 0.0033 decrease in crime rate.
B^4: A one-unit increase in the average sentence length corresponds to a small increase in crime rate, this is the only estimate where the result is not statistically significant (t = 1.36, p = 0.175).")


#c. It is highly likely that the above regression has endogeneity problems.

#Will fixed effects help with this problem? Why or why not?

#List and explain two factors that might be causing endogeneity issues for crime rate that are handled by entity fixed effects.
print(paste("Two factors that might be causing endogeneity are: 1. County Location, where the county is located in the state of North Carolina. This is important because the location of a county may impact the crime rates ocurring in each individual county. If the county is located by the sea, there could be a bigger crime rate because there may be a high population density (Just an Example). 2. County  This is a county fixed effect that is necessary because it includes the differences between individual counties, where the statistics would not. 2.  "))

#List and explain two factors that might be causing endogeneity issues for crime rate that are handled by time fixed effects.
print(paste())


#D. Repeat part b. with two way fixed effects. Run this regression, then show and interpret your regression results. What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean? Looking at these values, did the coefficients change a lot or a little? Do you think the fixed effects helped?  

Crime_Rate_fixed_effects_Results <- lm(Crime$crmrte ~ Crime$polpc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year), data = Crime)

summary(Crime_Rate_fixed_effects_Results)



#e. Unfortunately, there are likely still factors that are causing police to be endogenous.

#Using the weekly wage of local government employees, wloc, as an instrument, write down the first and second stage regression models.

#First Stage:

Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen,  data = Crime)

summary(Stage_One)

Instrument<- predict(Instrument_Model_Stage_One)

#Second Stage

Second_Stage <- lm(Crime$crmrte ~ Instrument + Crime$prbarr + Crime$prbconv + Crime$avgsen, data = Crime)

summary(Second_Stage)


#f. Adding to our model with two-way fixed effects, run a 2SLS regression where police is instrumented with the weekly wage of local government employees.

Fixed_Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year),  data = Crime)

summary(Fixed_Stage_One)

Fixed_Instrument<- predict(Instrument_Model_Stage_One)

#Second Stage

Fixed_Second_Stage <- lm(Crime$crmrte ~ Fixed_Instrument + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year), data = Crime)

summary(Fixed_Second_Stage)

#What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean?


#g. Assess the validity of the instrument:
  #Write down and explain the two conditions that instruments must satisfy.
print("Inclusion Restriction: An instrument (Z) needs to explain the endogenous variable of interest (X) – The instrumental variable needs to be relevant, it needs to be correlated with X, if not related the predicted X (x hat) would be a “bad” replacement for X. This is testable during the first stage regression by seeing if it meets the relevance condition (rule of thumb is t-stat for coefficient on Z > 3, if true we have irrelevant instrument, Z is sufficiently correlated with X)
Exclusion Restriction: An instrument (Z) needs to be uncorrelated with the error term (E). The instrument (Z) can only affect Y through the instrument's effect on X. If they are related, then the instrumental variable should instead be in the original regression as it would then be an omitted variable, causing bias. This is sometimes called the exogeneity condition and there is no way to formally test if this condition holds.")

  #Run the first stage regression. Report and interpret the t-statistic on the wloc variable.
  Fixed_Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year),  data = Crime)
  
  Fixed_Stage_One_summary <- summary(Fixed_Stage_One)
  t_stat_wloc <- Fixed_Stage_One_summary$coefficients["Crime$wloc", "t value"]
  print(t_stat_wloc)
print("The t-statistic on the wloc variable is -1.246517. This means the coefficient is around 1.25 standard errors below zero. This is a relatively small t-statistic and suggests that the estimated effect of the wloc variable may not be statistically significant.")

#Do you think that the weekly wage of local governments employees is a good instrument for the size of the police force in this context? Justify your answer.
print("I do not think the weekly wage of government employees is a good instrument for the size of the police force in this context. This is because the instrument does not meet the rule of thumb for the inclusion restriction for instruments, meaning the t-statistic for the coefficient on Z is not > 3. 1.25<3. This suggests the instrument is not relevant as it does not adequately explain the variable of interest. To be a valid instrument, it must meet both the inclusion and exclusion conditions. While wloc may meet the exclusion restriction, whether it does or does not is irrelevant as it does not meet the inclusion restriction.")
