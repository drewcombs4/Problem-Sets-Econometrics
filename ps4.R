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

print(paste("No, it does not make sense to call this a probabilitiy of arrest. In my own thoughts, I thoughts means the likelihood that a county has to arrest the common population. Without context, the variable is very ambiguous. In the way I percieved it, the variable's correlation with crime rate is highly perculiar due to it's negative relationship. Thus, in this study, the a higher probability of arrest a county has correlates with a county's lower crime rate. Economically, that statement does not really make sense. However, if the definition is changed, then it begins to make more sense. The more likely a county is to arrest people, the less likely people are to committ crimes. Unfortunetly, the highest probability of arrest occurs at a 2.75 probability, meaning that there is a 275% percent probability of arrest occurring in that county at that specific time. This specific probability of arrest does not make sense for the reason that they are 275% likely to arrest someone. I do not understand how that is even calculated. It does make sense that the county with the lowest crime rate in this study also has the highest probability of arrest at that specific time, when used as a deterrent."))

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


#List and explain two factors that might be causing endogeneity issues for crime rate that are handled by time fixed effects.

#D. Repeat part b. with two way fixed effects. Run this regression, then show and interpret your regression results. What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean? Looking at these values, did the coefficients change a lot or a little? Do you think the fixed effects helped?  




#e. Unfortunately, there are likely still factors that are causing police to be endogenous.

#Using the weekly wage of local government employees, wloc, as an instrument, write down the first and second stage regression models.

#f. Adding to our model with two-way fixed effects, run a 2SLS regression where police is instrumented with the weekly wage of local government employees.

#What are ˆβ1, ˆβ2, ˆβ3, ˆβ4? What do they mean?


#g. Assess the validity of the instrument:
  #Write down and explain the two conditions that instruments must satisfy.
  print(paste("A good instrument: 1. Is not correlated with and components on the error term, and 2. Explains the endogenous variable very well; it is highly correlated with X1."))

  #Run the first stage regression. Report and interpret the t-statistic on the wloc variable.
  
#Do you think that the weekly wage of local governments employees is a good instrument for the size of the police force in this context? Justify your answer.
