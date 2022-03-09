#####################################################
##
## Plotting Covid-19 cases in the U.S. based on 2016 voting records
## 
## Leilah Harouni
##
#####################################################

# Research question: 
# Do red and blue states have different levels of covid-19?
# Have they trended differently over time?

# Defining variables:
# States will be defined based on whether the majority voted for Trump/Clinton in 2016
# We will look at the total number of Covid-19 cases in states on the most recent day
# We will also adjust for state populations, estimated in 2019

# Load packages 
library(tidyverse)

# Load election data 
# codebook: https://github.com/MEDSL/2018-elections-unoffical/blob/master/election-context-2018.md

voting <- read.csv("/Users/lh689/Desktop/Github/covid/1976-2016-president.csv")

str(voting)
head(voting)

# Load covid-19 data
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

str(covid)
head(covid)

# Clean, organize and merge the data
# Only care about votes that went for Trump or Clinton
# There's something weird going on in New York state, so we want 
# to summarize across the multiple rows for Clinton/Trump votes in NY

# View(voting %>% filter(year == 2016 &state == "New York)) shows us that Trump and Clinton were voted in multiple times
# so we are going to want to sum those votes
# %in% means include
voting_small <- voting %>%
  select(year, state, state_fips, candidate, candidatevotes) %>%
  filter(year == 2016 & candidate %in% c('Trump, Donald J.', 'Clinton, Hillary')) %>%
  group_by(state, candidate) %>%
  summarize(candidate_votes = sum(candidatevotes))

# Filter the covid data set to include data ONLY on the most recent day
covid_small <- covid %>%
  mutate(date = as.Date(date)) %>%
  filter(date == max(date))

# Join the recent covid data with the voting data
joined_data <- voting_small %>%
  inner_join(covid_small, by = "state") %>%
  spread(candidate, candidate_votes) %>%
  rename(Clinton = `Clinton, Hillary`,
         Trump = `Trump, Donald J.`) %>%
  mutate(percent_trump = Trump/(Clinton + Trump),
         party = case_when(percent_trump > .5 ~ "Trump",
                           percent_trump < .5 ~ "Clinton"))

joined_data$`Clinton, Hillary`
# 'party' adds a column to joined data that identifies whether the 
# state went Trump/Clinton based on who got more votes

# Check to make sure the number of states for each is correct:
summary(as.factor(joined_data$party))

# On the most recent day, which party had more cases?
tapply(joined_data$cases, joined_data$party, sum)

# t test to see if the difference between the two numbers are significantly different
# they are not, p = .94
t.test(joined_data$cases ~ joined_data$party)

# Did percent voting for Trump predict cases?
# These are continuous variables so use a correlation or lm
model1 <- lm(cases ~ percent_trump, data = joined_data)
summary(model1)
# For every 1% increase that voted for trump, lead to 155985 fewer cases
# As more ppl vote for trump, there are fewer cases p = 0.02

# But we ignored population! 
# Now add a data set that includes state populations
population <- read.csv("/Users/lh689/Desktop/Github/covid/nst-est2019-alldata.csv")

# Select only the population estimate from 2019 and rename "NAME" to "state"
# so we can join it with the other data frames
pop_small <- population %>%
  select(NAME, POPESTIMATE2019) %>%
  rename(state = NAME)

head(pop_small)

# Join population with the data frame that includes political leaning
joined_data <- joined_data %>%
  inner_join(pop_small, by = "state")
# or can use merge

# Now let's control for population in our linear model...
model2 <- lm(cases ~ percent_trump + POPESTIMATE2019, data = joined_data)
summary(model2)


# Let's model the data over time, and look at how trends differ in red vs. blue states
# We also want to look at new cases each day (a more typical "curve"), and not
# total cases since the pandemic began

# First, let's create a "new_cases" variable in the covid df
covid <- covid %>%
  arrange(state, date) %>% # to make sure date is in right order
  group_by(state) %>%
  mutate(previous_day = lag(cases),
         new_cases = cases - previous_day)

# The add voting information to the "over time" data frame, as well as 
# population information. Then we can create a variable called "percent_infected"

# We eventually want to plot the number of NEW infections on each day, 
# but presented as a percent of the population 
# We will also plot separate lines for red and blue states
joined_over_time <- covid %>%
  inner_join(select(joined_data, party, state, POPESTIMATE2019), by = "state") %>%
  mutate(percent_infected = (new_cases/POPESTIMATE2019)*100)

joined_time_summary <- joined_over_time %>%
  group_by(date, party) %>%
  summarize(av_percent_infected = mean(percent_infected),
            se = sd(percent_infected)/sqrt(n()))

# Plot the data! Date on the x axis, av_infection_rate on y axis, and different
# lines for red vs. blue states
ggplot(joined_time_summary, aes(x = as.Date(date), y = av_percent_infected, group = party, color = party)) +
  geom_line() +
  geom_smooth() +
  labs(title = "Daily Confirmed COVID-19 Infections In Red and Blue States", x = "Date", 
       y = "Percent of Confirmed Infections In America") +
  scale_color_manual(values = c('Blue', 'Red'))

# width and height variables for saved plots
w = 6
h = 4

ggsave('/Users/lh689/Desktop/figures/COVID.png', width = w, height = h)