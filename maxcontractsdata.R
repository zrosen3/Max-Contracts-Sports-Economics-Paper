#load in required libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readxl)
library(fuzzyjoin)
library(sjPlot)


#load in salary data and stats da
setwd("~/Desktop/Sports Econ/Term Paper/Data Analysis/")
files <-list.files(pattern = "*.xlsx") 

advanced_stats <- read_excel(files[1])[-1,]
player_salaries <- read_excel(files[2])[-1,]
names(player_salaries) <- c("Rk", "Player", "Tm", "2020-21", "Signed Using")
stats <- left_join(player_salaries[-1,], advanced_stats, by = "Player")  
stats <- stats %>% distinct(Player, .keep_all=TRUE)
names(stats)[4] <- "salary"

#deadweight loss from price ceiling plot 
#dataframes 
supply <- data.frame(x = c(0,5), y = c(0, 5), Variable= "Supply")
demand <- data.frame(x = c(0, 5), y = c(5, 0), Variable = "Demand")
low_ceiling <- data.frame(x = c(0,5), y = 1.5, Variable = "Ceiling")
shortage <- data.frame(x = c(1.5, 3.5), y = 1.5, Variable= "Shortage")


#variables for graphs
line <- geom_line(aes(x=x, y = y, group = Variable, colour = Variable ))
theme <- theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

#price ceiling 
quantity_sold <- data.frame (x= 1.5, y = 1.5, Variable = "Quantity Sold")
ggplot(rbind(supply,demand, low_ceiling, shortage)) + 
  line+
  geom_point(data=quantity_sold, aes(x=x,y=y, group = Variable, colour = Variable)) +
  labs(x="Quantity", y = "Price", title = "Price Ceiling")+ 
  theme


stats <- stats %>% mutate(salarycap = 109140000, salary = as.numeric(salary), salarypercent = 100*(salary/salarycap), warp = 2.7 * VORP, min100 = 100 * MP)
#win shares vs. salary
ggplot(stats, aes (x = salarypercent, y = WS, color = salarycap))+ 
  geom_point() + 
  labs (x = "Salary as Percentage of Cap", y="Win Shares", title = "2021 NBA Win Shares vs. Salary" )+
  theme(legend.position = "none", text = element_text(size= 20), plot.title = element_text(hjust = 0.5))


#regress win shares on salary/cap, previous years win shares
regression1 <- lm(WS ~ PER + MP + salarypercent, data = stats)
summary(regression1)

stats_maxcontract <- stats %>% filter(salarypercent > 25)
regression2 <- lm(WS ~ PER + MP +  salarypercent, data = stats_maxcontract)
summary(regression2)

stats_nomaxcontract <- stats %>% filter(salarypercent < 25)
regression3 <- lm(WS ~ PER + MP + salarypercent, data = stats_nomaxcontract)
summary(regression3)


#regression tables 
predictors <- c("Intercept", "PER", "Minutes Played", "Salary as % of Cap")

#regression_table 
stats_table <- tab_model(regression1, regression2, regression3, 
  title = "Win Shares vs. Salary ",
  dv.labels = c("All Players", "Players with Salaries over 25% of Cap", "Players with Salaries under 23% of Cap"),
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  pred.labels = predictors,
  show.r2 = TRUE, 
  show.obs = TRUE,
  auto.label = FALSE)

stats_table


