###################################################
### Problem Set 3                               ###
### Spring 2025                                 ###
### Author: Drew Combs, Ben Abernathy           ###
###################################################
####### 
################
# INTRODUCTION #
#Load the dataset “baseball attendance.RData” into R and answer the following questions. 

#clear memory first
rm(list=ls())
#set working directory
cpath <- "/Users/drewcombs/desktop/Econometrics R Sessions"

setwd(cpath)
library(ggplot2)
load("baseball_attendance.RData")

baseball_attendance <- dta
rm(dta)

#a. What are the units of observation for this dataset? 

print(paste("Units of Observation: Major League Baseball Teams"))

#Compute the mean of home attendance for National League and American League Seperately

AmericanLeague <- subset(baseball_attendance,baseball_attendance$league == "AL", )

NationalLeague <- subset(baseball_attendance,baseball_attendance$league == "NL", )

mean(AmericanLeague$home_attend)

mean(NationalLeague$home_attend)

#Compute the mean runs scored for the American (AL) and National Leagues (NL) separately.

mean(AmericanLeague$runs_scored)

mean(NationalLeague$runs_scored)

#Generate separate scatterplots of attendance (on the y-axis) against runs scored (on the x-axis) for the leagues. 

#AL Chart
y_ALattendanceinmillions <- AmericanLeague$home_attend/1000000

AL_results_plot<- plot(AmericanLeague$runs_scored, y_ALattendanceinmillions, main= "Runs Impact on an AL Baseball Team's Seasonal Attendance",
     pch=16, col= "red", xlab = "Runs Scored by AL Teams per Season", ylim = c(0,5), ylab= "Total Attendance for AL Teams (in millions)")
abline(lm(y_ALattendanceinmillions~AmericanLeague$runs_scored), col= "black")

ggsave("AL_results_plot.pdf", plot = AL_results_plot, width = 8, height = 6)

#NL Chart
y_NLattendanceinmillions <- NationalLeague$home_attend/1000000

NL_results_plot <- plot(NationalLeague$runs_scored, y_NLattendanceinmillions, main= "Runs Impact on a NL Baseball Team's Seasonal Attendance",
     pch=16, col= "blue", xlab = "Runs Scored by NL Teams per Season", ylim = c(0,5), ylab= "Total Attendance for NL Teams (in millions)")
abline(lm(y_NLattendanceinmillions~NationalLeague$runs_scored), col= "black")

ggsave("NL_results_plot.pdf", plot = NL_results_plot, width = 8, height = 6)

#b. Assume that you are using a multivariate model: home attendancei = β0 + β1leaguei + β2runs scoredi + εi

#Run this regression then show and interpret your regression results. What are ˆβ0, ˆβ1, ˆβ2?
  #What do they mean?

Attendance_results <- lm(baseball_attendance$home_attend ~ baseball_attendance$league + baseball_attendance$runs_scored, data = baseball_attendance)

summary(Attendance_results)

print("Beta_0 is the expected value of home attendance for an MLB team that plays in the American League and scores 0 runs throughout an entire season. When a Major League Baseball team plays in AL and scores 0 runs, the total team attendance over the course of a season is -983734.8. Beta_1 represents the expected change in total attendance that occurs when comparing the two leagues, the AL to the NL, and holding runs to a constant of 0. If two teams, one AL and one NL, both score 0 runs throughout the entirety of a season, the home attendance of the NL is predicted to be greater than the reference group (AL) by 270694.3. In other terms, the NL is associated with higher attendance numbers than the AL. Beta_2 represents expected change in attendance numbers for every additional run a team scores, when holding the league constant. For every additional run an AL team scores, it is predicted that their attendance will increase by 3785.3 per run. If a team in the NL scores one additional run, then their increase is beta_1+ beta_2 * (# of addtional runs)")

#c. Generate a single scatterplot that has attendance (on the y-axis) and runs scored (on the x-axis).

#set the AL slope and intercept
AL_intercept <- Attendance_results$coefficients[1]
AL_slope <- Attendance_results$coefficients[3]

AL_convertedint <- AL_intercept/1000000
AL_convertedslope <- AL_slope/1000000

#set the NL slope and intercept
NL_intercept <- Attendance_results$coefficients[1]+Attendance_results$coefficients[2]
NL_slope <- Attendance_results$coefficients[3]

NL_convertedint <- NL_intercept/1000000
NL_convertedslope <- NL_slope/1000000
#Let's plot a scatter plot of this relationship and the predicted regression line 

y_Totattendanceinmillions <- baseball_attendance$home_attend/1000000

Attendance_results_plot <- ggplot(baseball_attendance, aes(x = runs_scored, y = y_Totattendanceinmillions)) +
  geom_point() +  

  geom_abline(intercept = NL_convertedint, slope = NL_convertedslope, size=1.1, color="blue")  + 
  geom_abline(intercept = AL_convertedint, slope = AL_convertedslope, size=1.1, color="red")  + 
  labs(title = "Attendance by MLB Teams Runs, NL and AL Interact",
       x = "Runs Scored",
       y = "Home Attendance (in Millions)") +
  theme_minimal()  # Use a minimal theme for aesthetics

#Plot the two regression lines produced by the analysis in part b. Remember to include axis labels and a title. 

print(Attendance_results_plot)

# Save the plot
ggsave("Attendance_results_plot.pdf", plot = Attendance_results_plot, width = 8, height = 6)

#d. Repeat part b. with an interaction term between runs scored and league. 

#Interactive Regression  
Attendance_Runs_interact_results <- lm(baseball_attendance$home_attend ~ baseball_attendance$league + baseball_attendance$runs_scored + baseball_attendance$runs_scored:baseball_attendance$league, data=baseball_attendance)

summary(Attendance_Runs_interact_results)

# What are ˆβ0, ˆβ1, ˆβ2, ˆβ3? What do they mean? Do you think we should keep the interaction term?


  #e. Next, examine the relationship between log-attendance and runs scored controlling for the number of wins:
  
  #ln(home attendancei) = β0 + β1leaguei + β2runs scoredi + β3winsi + εi

#Run this regression and show your regression results. 

log_model_attendance <- lm(log(baseball_attendance$home_attend) ~ baseball_attendance$league + baseball_attendance$runs_scored + baseball_attendance$wins, data = baseball_attendance)

# View the summary of the model
summary(log_model_attendance)

#Explain what ˆβ1, ˆβ2, and ˆβ3 mean in this regression and interepret the results.


#f. Now, let’s examine the relationship between log-attendance and the standardized values of runs scored and wins. Hint: Review the Chapter 5 slides if you’ve forgotten what this means.

#ln(home attendancei) = β0 + β1leaguei + β2runs scored standi + β3wins standi + εi

#Run this regression and show your regression results. 

#Standardizing the Values of Runs Scored
runs_scored_standardized <- (baseball_attendance$runs_scored - mean(baseball_attendance$runs_scored)) / sd(baseball_attendance$runs_scored)

#Standardizing the Values of Wins
wins_standardized <- (baseball_attendance$wins - mean(baseball_attendance$wins)) / sd(baseball_attendance$wins)

#Regression
log_model_attendance_standardized <- lm(log(baseball_attendance$home_attend) ~ baseball_attendance$league + runs_scored_standardized + wins_standardized, data = baseball_attendance)

summary(log_model_attendance_standardized)

#Explain what ˆβ1, ˆβ2, and ˆβ3 mean in this regression. Interpret the Regression results.


#g. Does runs scored have a greater effect on home attendance than wins?


#Write down the null and alternative hypotheses that correspond to this question.
print("Null Hypothesis H0: Runs scored do not have a greater effect on home attendance than wins Bruns <= Bwins")
print("Alternative Hypothesis HA: Runs scored do have a greater effect on home attendance than wins Bruns > Bwins")

#Test your hypothesis at the 0.95 significance level, then report and interpret your results.


#Explain why standardizing the variables allows for a direct comparison of the effects of runs scored and wins on home attendance.

