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

#b. Assume that you are using a multivariate model: crmrteit = β0 + β1polpcit + β2prbarrit + β3prbconvit + β4avgsenit  εi

#Run this regression then show and interpret your regression results. 

Crime_Rate_Results <- lm(Crime$crmrte ~ Crime$polpc + Crime$prbarr + Crime$prbconv + Crime$avgsen , data = Crime)

summary(Crime_Rate_Results)

#What are β0, ˆβ1, ˆβ2, ˆβ3, ˆβ4?
  
#What do they mean?

print("Beta_0 ")


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

  #Run the first stage regression. Report and interpret the t-statistic on the wloc variable.
  Fixed_Stage_One <- lm(Crime$polpc ~ Crime$wloc + Crime$prbarr + Crime$prbconv + Crime$avgsen + factor(county) + factor(year),  data = Crime)
  
  Fixed_Stage_One_summary <- summary(Fixed_Stage_One)
  t_stat_wloc <- Fixed_Stage_One_summary$coefficients["Crime$wloc", "t value"]
  print(t_stat_wloc)
#Do you think that the weekly wage of local governments employees is a good instrument for the size of the police force in this context? Justify your answer.
