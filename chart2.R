# chart2.R
df <- data.frame(variable1 = rnorm(100), variable2 = rnorm(100), variable3 = rnorm(100), variable4 = rnorm(100), gender = rep(c("Male", "Female"), length.out = 100))
chart2_1 <- ggplot(df, aes(x = variable1, y = variable2, color = gender)) + geom_point() + theme_minimal()
chart2_2 <- ggplot(df, aes(x = variable3, y = variable4, color = gender)) + geom_point() + theme_minimal()






library(dplyr)
library(ggplot2)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Satterplot of comparing Total Pop 15 to 64 to Total Pop in Jail 

f_totalpop <- incarceration_data %>%
  filter(year == max(year)) %>%
  mutate(percentage_pop_jail_female = (female_jail_pop/(female_jail_pop + male_jail_pop))*100) %>%
  mutate(percentage_female_pop = (female_pop_15to64/total_pop_15to64)*100) %>%
  select(county_name, percentage_pop_jail_female, percentage_female_pop)

m_totalpop <- incarceration_data %>%
  filter(year == max(year)) %>%
  mutate(percentage_pop_jail_male = (male_jail_pop/(male_jail_pop + female_jail_pop))*100) %>%
  mutate(percentage_male_pop = (male_pop_15to64/total_pop_15to64)*100) %>%
  select(county_name, percentage_pop_jail_male, percentage_male_pop)

library(ggplot2)

chart2_1 <- ggplot(f_totalpop) +
  geom_point(aes(x=percentage_pop_jail_female, y = percentage_female_pop), color="darkred") +
  ylim(0,100) +
  ggtitle("Proportion of Female Population in County and in jaill") +
  ylab("percentage of Female Population in County, ages of 15-64") +
  xlab("Percentage of Female Population in Jail")

chart2_2 <- ggplot(m_totalpop) +
  geom_point(aes(x=percentage_pop_jail_male, y = percentage_male_pop),  color = "steelblue") +
  ylim(0,100) +
  ggtitle("Proportion of Male Population in County and in Jail") +
  ylab("percentage of Male Population in County, Ages of 15-64") +
  xlab("Percentage of Male Population in Jail")


