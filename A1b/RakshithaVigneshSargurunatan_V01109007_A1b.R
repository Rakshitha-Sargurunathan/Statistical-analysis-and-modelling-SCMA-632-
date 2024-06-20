Aggignment A1b - DESCRIPTIVE STATISTICS : PREPARATION AND ANALYSIS OF DATA
  *AUTHOR: Rakshitha vignesh Sargurunathan
  *VID   : V01109007
  *DATE  : 18/06/2024

#To set the workspace
setwd('E:\\VCU\\SCMA\\DATA')
#To check workspace
getwd()

library(dplyr)

ipl_ball=read.csv("IPL_ball_by_ball_updated till 2024.csv")
salary = read.csv("IPL SALARIES 2024.xlsx")
head(ipl_ball)

#1) Arrange the data IPL round-wise and batsman, ball, runs, and wickets per player per match.

ipl_ball_roundwise=ipl_ball %>%
  arrange(Season,`Match.id`)  
head(ipl_ball)
tail(ipl_ball)

#Arrange the data for each batsman
batsman_data=ipl_ball%>%
  group_by(Season,Striker) %>%
  summarise(
    Total_Balls_Faced =n(),
    Total_Runs_Scored=sum(runs_scored),
    .groups='drop' 
  )
str(batsman_data)

#arrange the data for each bowler
bowler_data=df%>%
  filter(wicket_confirmation==1) %>%
  group_by(Match.id, Date,Bowler) %>%
  summarise(
    Total_Wickets_Taken=n(),
    .groups='drop'
  )
str(bowler_data)

# 2) Indicating the top three run-getters and top three wicket-takers in each IPL round.

# Top 3 run-getters in each IPL season
top_run_getters = df %>%
  group_by(Season, Striker) %>%  
  summarise(Total_Runs = sum(runs_scored)) %>%  
  arrange(Season, desc(Total_Runs)) %>% 
  group_by(Season) %>%  
  slice_head(n = 3) %>%  
  ungroup()  

print("Top Three Run Getters in Each IPL Season:")
print(top_run_getters)


# Top 3 wicket-takers in each IPL season
top_wicket_takers= df %>%
  filter(wicket_confirmation == 1) %>%  
  group_by(Season, Bowler) %>%  
  summarise(Total_Wickets_Taken = n()) %>%  
  arrange(Season, desc(Total_Wickets_Taken)) %>%  
  group_by(Season) %>%  
  slice_head(n = 3) %>%  
  ungroup()  

print("Top Three Wicket Takers in Each IPL Season:")
print(top_wicket_takers)

# 3) Fitting the most appropriate distribution for runs scored and wickets taken by the top three batsmen 
#and bowlers in the last three IPL tournaments.

install.packages("MASS")
library(MASS)

# Fit distributions for runs scored by top batsmen
# Normal distribution
runs_data<- top_run_getters %>%
  filter(Striker %in% top_run_getters) %>%
  select(Total_Runs)
runs_fit= fitdistr(runs_data$Runs, "normal")

print("Parameters of Normal Distribution for Runs Scored:")
print(runs_fit$estimate)

wickets_data <- top_wicket_takers %>%
  select(Player, Total_Wickets_Taken)

# Poisson distribution
wickets_fit <- fitdistr(wickets_data$Total_Wickets_Taken, "poisson")

cat("\nParameters of Poisson Distribution for Wickets Taken:\n")
print(wickets_fit$estimate)

install.packages("e1071")
library(dplyr)
library(MASS)   
library(e1071) 

# 4 ) Fitting Distribution for Mohammed Siraj

Siraj_striker_runs=df%>%
  filter(Striker == "Mohammed Siraj")

hist(Siraj_striker_runs$runs_scored, breaks = 10, freq = FALSE, main = "Histogram of Mohammed Siraj's Runs as Striker")
lines(density(Siraj_striker_runs$runs_scored), col = "Red")

# Normality test
shapiro_test <- shapiro.test(Siraj_striker_runs$runs_scored)
cat("Shapiro-Wilk Normality Test:\n")
print(shapiro_test)

skew <- skewness(Siraj_striker_runs$runs_scored)
kurt <- kurtosis(Siraj_striker_runs$runs_scored)
cat("\nSkewness:", skew, "\n")
cat("Kurtosis:", kurt, "\n")

# Normal distribution
params <- fitdistr(Siraj_striker_runs$runs_scored, "normal")

cat("\nParameters of Normal Distribution for Mohammed Siraj's Runs as Striker:\n")
print(params$estimate)

# Kolmogorov-Smirnov test
ks_test <- ks.test(Siraj_striker_runs$runs_scored, "pnorm", mean=params$estimate[1], sd=params$estimate[2])
cat("\nKolmogorov-Smirnov Test for Normality:\n")
print(ks_test)

----
# 5 )Relationship between a player’s performance and the salary he gets.
ipl_salary=salary %>%
  mutate(Matched_Player = sapply(Player, function(x) match_names(x, total_run_each_year$Striker)))

ipl_salary=salary[!is.na(salary$Matched_Player), ]

df_merged=merge(salary, total_run_each_year, by.x = "Matched_Player", by.y = "Striker")

# Correlation between Salary and Runs
correlation= cor(df_merged
                 runs_scored, use = "complete.obs")

print(paste("Correlation between Salary and Runs:", correlation))