###################################################
### Final Project                               ###
### Spring 2025                                 ###
### Author: Drew Combs.                         ###
###################################################
####### 
################
# INTRODUCTION #

#clear memory first
rm(list=ls())

#set working directory
cpath <- "/Users/drewcombs/Desktop/Econometrics_R_Sessions/"
setwd(cpath)

#install.packages("writexl")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("car")
#install.packages("ggimage")
library(car)
library(readxl)
library(writexl)
library(ggplot2)
library(teamcolors)
library(ggimage)

#NBA_Data <- read_excel("~/NBAData_Open.xlsx")
NBA_Data <- read_excel("/Users/drewcombs/Desktop/Econometrics_R_Sessions/NBAData.xlsx")

#Regression Actual

#Renaming Data
Wins <- NBA_Data$W
ThreePointPercentage <- NBA_Data$`3P%`
ThreePointAttempts <- NBA_Data$`3PA`
TwoPointPercentage <- NBA_Data$`2P%`
TwoPointAttempts <- NBA_Data$`2PA`
Turnovers <- NBA_Data$TOV
Offensive_Rebounds <- NBA_Data$ORB
Assists <- NBA_Data$`AST`
Weighted3PEff <- ThreePointPercentage * ThreePointAttempts
Weighted2PEff <- TwoPointPercentage * TwoPointAttempts

# Demean total stats by year
NBA_Data$Weighted3PEff_Demeaned <- with(NBA_Data,
                                        Weighted3PEff - ave(Weighted3PEff, Year, FUN = mean))

NBA_Data$TwoPointEff_Demeaned <- with(NBA_Data,
                                      Weighted2PEff - ave(Weighted2PEff, Year, FUN = mean))

NBA_Data$AST_Demeaned <- with(NBA_Data, AST - ave(AST, Year, FUN = mean))

NBA_Data$TOV_Demeaned <- with(NBA_Data, TOV - ave(TOV, Year, FUN = mean))

NBA_Data$ORB_Demeaned <- with(NBA_Data, ORB - ave(ORB, Year, FUN = mean))

NBA_Data$STL_Demeaned <- with(NBA_Data, STL - ave(STL, Year, FUN = mean))

NBA_Data$BLK_Demeaned <- with(NBA_Data, BLK - ave(BLK, Year, FUN = mean))


# Regression Model
model_defense <- lm(Wins ~ Weighted3PEff_Demeaned + TwoPointEff_Demeaned +
                      TOV_Demeaned + ORB_Demeaned + AST_Demeaned +
                      STL_Demeaned + BLK_Demeaned + factor(Team),
                    data = NBA_Data)

summary(model_defense)



#SCATTER PLOTS FOR 3-POINT PERCENTAGE

# Ensure the Year column is a factor
NBA_Data$Year <- as.factor(NBA_Data$Year)

# Define logo URLs and local Bobcats path
team_logo_urls <- c(
  "Atlanta Hawks" = "https://a.espncdn.com/i/teamlogos/nba/500/atl.png",
  "Boston Celtics" = "https://a.espncdn.com/i/teamlogos/nba/500/bos.png",
  "Brooklyn Nets" = "https://a.espncdn.com/i/teamlogos/nba/500/bkn.png",
  "Charlotte Hornets" = "https://a.espncdn.com/i/teamlogos/nba/500/cha.png",
  "Charlotte Bobcats" = "/Users/drewcombs/Desktop/Econometrics_R_Sessions/Charlotte_Bobcats.png",  # Local path here
  "Chicago Bulls" = "https://a.espncdn.com/i/teamlogos/nba/500/chi.png",
  "Cleveland Cavaliers" = "https://a.espncdn.com/i/teamlogos/nba/500/cle.png",
  "Dallas Mavericks" = "https://a.espncdn.com/i/teamlogos/nba/500/dal.png",
  "Denver Nuggets" = "https://a.espncdn.com/i/teamlogos/nba/500/den.png",
  "Detroit Pistons" = "https://a.espncdn.com/i/teamlogos/nba/500/det.png",
  "Golden State Warriors" = "https://a.espncdn.com/i/teamlogos/nba/500/gs.png",
  "Houston Rockets" = "https://a.espncdn.com/i/teamlogos/nba/500/hou.png",
  "Indiana Pacers" = "https://a.espncdn.com/i/teamlogos/nba/500/ind.png",
  "Los Angeles Clippers" = "https://a.espncdn.com/i/teamlogos/nba/500/lac.png",
  "Los Angeles Lakers" = "https://a.espncdn.com/i/teamlogos/nba/500/lal.png",
  "Memphis Grizzlies" = "https://a.espncdn.com/i/teamlogos/nba/500/mem.png",
  "Miami Heat" = "https://a.espncdn.com/i/teamlogos/nba/500/mia.png",
  "Milwaukee Bucks" = "https://a.espncdn.com/i/teamlogos/nba/500/mil.png",
  "Minnesota Timberwolves" = "https://a.espncdn.com/i/teamlogos/nba/500/min.png",
  "New Orleans Pelicans" = "https://a.espncdn.com/i/teamlogos/nba/500/no.png",
  "New York Knicks" = "https://a.espncdn.com/i/teamlogos/nba/500/ny.png",
  "Oklahoma City Thunder" = "https://a.espncdn.com/i/teamlogos/nba/500/okc.png",
  "Orlando Magic" = "https://a.espncdn.com/i/teamlogos/nba/500/orl.png",
  "Philadelphia 76ers" = "https://a.espncdn.com/i/teamlogos/nba/500/phi.png",
  "Phoenix Suns" = "https://a.espncdn.com/i/teamlogos/nba/500/phx.png",
  "Portland Trail Blazers" = "https://a.espncdn.com/i/teamlogos/nba/500/por.png",
  "Sacramento Kings" = "https://a.espncdn.com/i/teamlogos/nba/500/sac.png",
  "San Antonio Spurs" = "https://a.espncdn.com/i/teamlogos/nba/500/sa.png",
  "Toronto Raptors" = "https://a.espncdn.com/i/teamlogos/nba/500/tor.png",
  "Utah Jazz" = "https://a.espncdn.com/i/teamlogos/nba/500/utah.png",
  "Washington Wizards" = "https://a.espncdn.com/i/teamlogos/nba/500/wsh.png"
)

# Assign team logos to your dataframe
NBA_Data$Logo <- team_logo_urls[NBA_Data$Team]

# Check the file exists for Bobcats if you're getting errors (helped by AI)
if (!file.exists("/Users/drewcombs/Desktop/Econometrics_R_Sessions/Charlotte_Bobcats.png")) {
  stop("Bobcats logo is missing or incorrectly named.")
}

#Plot for Per Year
three_point_pct_plot_per_year <- ggplot(NBA_Data, aes(x = `3P%`, y = Wins)) +
  geom_image(aes(image = Logo), size = 0.05) +
  geom_text(aes(label = `3P%`), vjust = -1, size = 1.8, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~Year) +
  labs(
    title = "Three Point Percentage vs Wins by Year 2014–2024",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

print(three_point_pct_plot_per_year)
ggsave("three_point_pct_plot_per_year.pdf", plot = three_point_pct_plot_per_year, width = 10, height = 8)

#3-PCT by Every Team
three_point_pct_plot_per_team <- ggplot(NBA_Data, aes(x = Three_Point_Makes / Three_Point_Attempted, y = Wins)) +
  geom_image(aes(image = Logo), size = 0.08) +
  geom_text(aes(label = Year), vjust = -1, size = 1, color = "black") +  # Add year text near each point
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~Team) +
  labs(
    title = "Three Point Percentage vs Wins by Team 2014-2024",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

print(three_point_pct_plot_per_team)
ggsave("three_point_pct_plot_per_team.pdf", plot = three_point_pct_plot_per_team, width = 12, height = 10)


#3-PT PCT Total
three_point_pct_plot <- ggplot(NBA_Data, aes(x = Three_Point_Makes / Three_Point_Attempted, y = Wins)) +
  geom_image(aes(image = Logo), size = 0.04) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Three Point Percentage vs Wins 2014-2024",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

print(three_point_pct_plot)
ggsave("three_point_pct_plot.pdf", plot = three_point_pct_plot, width = 8, height = 6)
