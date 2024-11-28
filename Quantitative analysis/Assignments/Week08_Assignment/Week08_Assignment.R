#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 8 Homework Exercise: Survey Vignettes


# DATA EXPLANATION

# For this week's exercise, we will be looking at data from a 2004 paper by 
# Gary King, Christopher L.J. Murray, Joshua A. Salomon, and Ajay Tandon, titled
# "Enhancing the validity and cross-cultural comparability of measurement in
# survey research", published in American Political Science Review.

# (You've already read some of Gary King's work - he was the co-author of the
# book chapters from Designing Social Inquiry you read at the start of the term.)

# In this study, King et al carried out a series of surveys in both Mexico and
# China. They asked four questions - a self-assessment question, and then 
# three vignettes.

# 1) How much say do you have in getting the government to address the issues
#    that interest you?

# 2) [Alison] lacks clean drinking water. She and her neighbours are supporting a
#    candidate from the opposition party in the forthcoming elections who has 
#    promised to address the issue. It appears that so many people in her area
#    feel the same way that the opposition candidate will defeat the incumbent
#    government representative.
#    How much say does Alison have in getting the government to address the issues
#    that interest her?

# 3) [Jane] lacks clean drinking water because the government is pursuing an
#    industrial development plan. In the campaign for the upcoming election,
#    an opposition party has promised to address the issue, but she feels it
#    would be futile to vote for the opposition since the government is certain
#    to win anyway.
#    How much say does Jane have in getting the government to address the issues
#    that interest her?

# 4) [Moses] lacks clean drinking water. He would like to change this, but he 
#    can't vote, and feels that no one in the government cares about this issue.
#    So he suffers in silence, hoping that something will be done in the future.
#    How much say does Moses have in getting the government to address the issues
#    that interest him?

# The possible responses were:
# (1) No say at all; (2) Little say; (3) Some say; (4) A lot of say; (5) Unlimited say

# In addition, for each respondent, their age and their country is recorded.

# This data can be found in the file "pol_survey.csv".



# YOUR ASSIGNMENTS

# We are interested in testing the following hypotheses with this data:

# H1) People's rating of their own political power ("self") depends on
#     their age and their country of residence.

# H2) Depending on their country of residence, the influence of a person's
#     age on their perceived political power is different.

# H3) A person's perception of their own political power will also influence
#     their response to the vignettes, as will their age and country.

# H4) Depending on a person's country, the influence of their self-perception
#     on how they respond to the vignettes will change.

# H5) Depending on a person's age, the influence of their self-perception on
#     how they respond to the vignettes will change.



# In practical terms, this means you should do the following:

# - Import the CSV file. Check the data and ensure that it is as expected.
#   If there is any variable that should be categorical and isn't, convert it.

rm(list = ls())
library(tidyverse)
library(stargazer)

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/pol_survey.csv"
df <- read.csv(url)

# - Run a linear model to test the influence of a person's age and country on
#   their perception of their own political power.

model1 <- lm(self ~ age + country, data = df)
stargazer(model1, type = "text")


# - Run another linear model to test whether age and country interact in their
#   influence on self-perception of political power.

model2 <- lm(self ~ age * country, data = df)
stargazer(model2, type = "text")

# - Next, pick one of the vignettes (Alison, Jane, or Moses). You don't have to
#   do all three, but it might be interesting to check how the results differ.

# - For that vignette, run a linear model to see how age, country, and self-
#   perceived political power impact the response to the vignette.

model3 <- lm(alison ~ age + country + self, data = df)
stargazer(model3, type = "text")

# - Next, test two possible interaction terms - self with age, and self with country.

model4 <- lm(alison ~ age * self + country * self, data = df)
stargazer(model4, type = "text")



# - For any interaction effect you find that is statistically significant, try
#   to plot an interaction chart. Write a couple of short sentences summarising
#   what you think the interaction shows.



# Fixing the country character variable and adding a new response variable
# with the different factored responsese to the numerical values
df <- df %>%
  mutate(
    country = factor(country),
    response = case_when(
      self == 1 ~ "No say at all",
      self == 2 ~ "Little say",
      self == 3 ~ "Some say",
      self == 4 ~ "A lot of say",
      self == 5 ~ "Unlimited say"
    ),
    response = factor(
      response,
      levels = c("No say at all", "Little say", "Some say", "A lot of say", "Unlimited say"),
      ordered = TRUE
    )
  )

# Plotting the interaction between self and country on alison
df %>%
ggplot(aes(x = response, y = alison, color = country)) +
  geom_point() +
  geom_smooth(aes(x = as.numeric(response),y=alison), method = "lm") +
  labs(title = "Interaction between Self and Country on Alison Score",
       x = "",
       y = "Alison Score",
       color= "Country") +
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("palevioletred", "cornflowerblue"))

# The linear model shows that the coefficient *self* does not have any statistically
# significant effect on alison by itself, however when we include the interaction
# term *self* with *country* we see that the coefficient for the interaction term
# then becomes statistically significant. This means that between China and Mexico
# the effect of self on alison is different. In Mexico as self increases the alison
# score increases more compared to China.

# In summary the analysis shows that the effect of self belief on power
# to address issues that interest you on the vignette alison is culturally 
# different between China and Mexico.


# - Finally, state what you think the most effective model to explain the response
#   to the vignette you chose is, and give your reasoning in a couple of sentences.

stargazer(model3, model4, type = "text")


# The most effective model to explain the response to the vignette is model 4
# because Adjusted R-squared is marginally higher than model 3 which
# means that the model explains the data slightly better.

# Model 4 includes interaction terms between self and age and self and country,
# this included the significant interaction between self and country which
# suggests that the effect of self on alison is different between China and Mexico.
# We could not see this in model 3.

# Also model 4 has a slighly lower Residual Standard Error which means that the
# model has a better fit to the data.

# In conclusion model 4 has improved model fit, includes significant interaction
# and has a lower residual standard error which makes it the most effective model.

