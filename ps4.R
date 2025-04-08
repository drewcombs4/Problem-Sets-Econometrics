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
install.packages("Ecfun")
library(Ecdat)
library(Ecfun)
install.packages("ggplot2")
library(ggplot2)
data("Crime" , package ="Ecdat")

#a. What are the units of observation for this dataset?
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
print(paste("Two factors that might be causing endogeneity are: 1. County Location and 2. County Local Culture. 
            County Location is where the county is located in the state of North Carolina. This is important because the location of a county may impact the crime rates occurring in each individual county.  Differences in geography are not observed in the data, but may effect the crime rates differently between counties. For example, if a county is located by the sea, there could be a bigger crime rate because there may be a high population density as compared to rural (Just an Example, but population density is located in our data). The geography can be unobserved in the data, which could impact the local culture as well. To account for this, I added a singular county factor which takes into account the fixed differences for each individual county. This county fixed effect is necessary because it includes the differences between individual counties, where the observed statistics would not."))

#List and explain two factors that might be causing endogeneity issues for crime rate that are handled by time fixed effects.
print(paste("Two factors that might be causing endogeneity are: 1. Year and 2. National Unemployment Rates. 
            Year is the year that data was observed in. This is important because the year of the date may show differing effects on crime rates as it is not observed in the data. The specific year that is recorded may have significant yearly events that impact the crime rates occurring in each individual county. For instance, the national unemployment rate will be an indicator on possible county level crime levels, but is not observed in our data.. To account for this, I added a singular year factor which takes into account the fixed differences for each individual year. This time fixed effect is necessary because it includes the differences between individual years, where the observed statistics would not. "))


#D. Repeat part b. with two way fixed effects. Run this regression, then show and interpret your regression results. What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean? Looking at these values, did the coefficients change a lot or a little? Do you think the fixed effects helped?  

Crime_Rate_fixed_effects_Results <- lm(Crime$crmrte ~ Crime$polpc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year), data = Crime)

summary(Crime_Rate_fixed_effects_Results)

#ˆβ1 : For each additional unit of police per capita, the crime rate increases by 2.135 holding other variables constant.
#ˆβ2 : For each 1-unit increase in the probability of arrest, the crime rate decreases by 0.0007 holding other variables constant.
#ˆβ3 : For each 1-unit increase in the probability of conviction, the crime rate decreases by .00009 holding other variables constant.
#ˆβ4 : For each additional year of average sentence length, the crime rate increases by 0.000008 holding other variables constant.

#Looking at these values, did the coefficients change a lot or a little? 
print("All of the coefficents decreased as a result of adding the fixed effects in. Moreover, the probability of arrest became less statistically significant as a result of factroing out the variables.")
#Do you think the fixed effects helped?  
  print("The fixed effects made our data less precise, but also less bias. I am unsure if it really helped us to fully see this correlation or not.")
#e. Unfortunately, there are likely still factors that are causing police to be endogenous.

#Using the weekly wage of local government employees, wloc, as an instrument, write down the first and second stage regression models.

#First Stage:

Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen,  data = Crime)

summary(Stage_One)

Instrument<- predict(Stage_One)

#Second Stage

Second_Stage <- lm(Crime$crmrte ~ Instrument + Crime$prbarr + Crime$prbconv + Crime$avgsen, data = Crime)

summary(Second_Stage)


#f. Adding to our model with two-way fixed effects, run a 2SLS regression where police is instrumented with the weekly wage of local government employees.

Fixed_Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year),  data = Crime)

summary(Fixed_Stage_One)

Fixed_Instrument<- predict(Fixed_Stage_One)

#Second Stage

Fixed_Second_Stage <- lm(Crime$crmrte ~ Fixed_Instrument + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year), data = Crime)

summary(Fixed_Second_Stage)

#What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean?

#ˆβ1 : For each additional unit of police per capita, after factoring out the wages of the local government's affect on the size of the police force, the crime rate increases by 1.499 holding other variables constant.
#ˆβ2 : For each 1-unit increase in the probability of arrest, the crime rate decreases by 0.0005 holding other variables constant.
#ˆβ3 : For each 1-unit increase in the probability of conviction, the crime rate decreases by .00006 holding other variables constant.
#ˆβ4 : For each additional year of average sentence length, the crime rate increases by 0.000003 holding other variables constant.

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

#Short Answer 


#a.
print("The three limitations to hypothesis testing that were discussed in class are that hypothesis testing is useless if X is endogenous, hypothesis testing is hard to do with small sample sizes, and, depending on the circumstance, substantive (economic) difference may be more important than statistical difference. If X is endogenous, it means X is correlated with the error term in the model. If X and the error term are correlated, it will not be possible to accurately determine how, what direction, or even if it is truly X affecting Y, thereby making the results of the hypothesis test useless. With small sample sizes hypothesis, testing becomes more difficult to do with smaller sample sizes as smaller sample sizes, the mean and standard deviation become less stable, which can lead to distortion of the t-statistics and p-values as the smaller the sample size, the less likely it is to be representative of the population. Because of this smaller sample size, the hypothesis test will have less certainty and may more often result in a Type II error (Ho is false but we fail to reject it). Hypothesis testing tests for statistical significance, i.e. is a given effect likely to have occurred by pure chance. Economic significance asks the question of is any given effect is important, relevant, or meaningful based on the context and scale – in essence, ‘Does it matter?’. In a large dataset, economically small effects may have statistical significance, while in small datasets, economically large effects may lack statistical significance.") 

#b.
print("i. 45.1 gallons per capita
ii. 31.65 gallons per capita
iii. 42.5 gallons per capita
iv. 13.8 gallons per capita")

#c.
print("The two model specification traps that were discussed are model fishing and p-hacking. Model fishing is reworking a model until you find the results you are looking for, while p-hacking is reworking a model until you get statistically significant results. The two traps are similar in that they involve altering how you mold or interpret your data in order to get specific results. Both traps can also be avoided through similar means; to avoid these traps, you need to choose your model before getting the results and be transparent about why you chose the model you are presenting. You should also document and explain any changes you may have made to the model and why. Presenting the results from multiple model specifications also provides the reader with more information so they can ultimately make up their own mind about the results presented.")
