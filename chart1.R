# chart1.R
library(ggplot2)
df <- data.frame(year = 2000:2010, count = rnorm(11, 100, 10), gender = rep(c("Male", "Female"), length.out = 11))
chart1 <- ggplot(df, aes(x = year, y = count, color = gender)) + geom_line() + theme_minimal()









'''{r, echo = TRUE}
library(ggplot2)
source("summary.R")




library(ggplot2)
library(dplyr)
library(tidyr)
library("scales")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Chart 1: Trends overtime -> Line plot of sum total imprisoned over time by gender

sum_female_year <- incarceration_data %>%
  group_by(year) %>%
  summarize(female = sum(female_jail_pop, na.rm = TRUE))


sum_male_year <- incarceration_data %>%
  group_by(year) %>%
  summarize(male = sum(male_jail_pop, na.rm = TRUE))


sum_f_m_year <- left_join(sum_female_year, sum_male_year, by = "year") %>%
  pivot_longer(col= c("female", "male"),
               names_to = "gender")

chart1 <- ggplot(sum_f_m_year) +
  geom_line(aes(x=year, y=value, group = gender, colour = gender)) +
  ggtitle("Total Number of People in Jail by Gender Overtime") + 
  ylab("Number of People") + 
  xlab("Year") +
  scale_y_continuous(labels =scales::comma)



