#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 10 (Winter Break) Homework Exercises
# Load the data
options(scipen = 999)
rm(list = ls())
library(tidyverse)
library(stargazer)
url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/grad_income_2.csv"
grad_income <- read.csv(url)

# Check the data
summary(grad_income)

# EXERCISE 1: Curvilinear Relationships

# 1) Find the most appropriate and effective model for this data. 
# Show your workings in your R code (i.e what other models you tried, and any tests you did to compare models.)

# Fit a linear model
linear_model <- (lm(income ~ age, data = grad_income))

# Check the model with stargazer
stargazer(linear_model, type = "text")


# GGplot the model
grad_income %>% 
  ggplot(aes(x = age, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

# Looking at the quadratic model

# Fit a quadratic model
grad_income$gender <- factor(grad_income$gender)


grad_income$age2 <- grad_income$age^2
quad_model <- lm(income ~ age + age2, data = grad_income)


# Check both models with stargazer
stargazer(linear_model, quad_model, type = "text")

# GGplot the quadratic model
grad_income %>%
  ggplot(aes(x = age, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2), 
              color = "red")+
  theme_minimal()

# Continuing with looking at cubic model

poly_model <- lm(income ~ poly(age, 3), data = grad_income)

# Check all models with stargazer
stargazer(linear_model, quad_model, poly_model, type = "text")

# GGplot the cubic model

grad_income %>%
  ggplot(aes(x = age, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              color = "red")+
  theme_minimal()

# Code copied from week 09 R assignment to look for the best polynomial model
poly_results <- tibble(
  Level = numeric(),
  AdjR2 = numeric(),
  Significance = numeric(),
  Stars = character()
)
for (polycount in 10:3) {
  fit <- lm(income ~ poly(age, polycount), data = grad_income)
  signif <- summary(fit)$coefficients[polycount + 1, 4]
  poly_results <- add_row(poly_results, 
                          Level = polycount,
                          AdjR2 = summary(fit)$adj.r.squared,
                          Significance = signif,
                          Stars = case_when(
                            signif < 0.01 ~ "***",
                            signif < 0.05 ~ " **",
                            signif < 0.1 ~ "  *",
                            TRUE ~ "   "
                          ))
}
poly_results

# The linear model is the worst, while the best model seems to be the cubic model
#with a slightly higher adjusted R-squared value and a lower p-value than the quadratic model.

# Lets continue and look at the log and exp model

# Fit a exp
exp_model <- lm(log(income) ~ age, data = grad_income)

# Fit a log model
log_model <- lm(income ~ log(age), data = grad_income)

# log log model
log_log_model <- lm(log(income) ~ log(age), data = grad_income)

# Check poly model with stargazer
stargazer(exp_model, log_model,log_log_model, type = "text")

# Log and exp models are worse as I expected.

# Lastly looking at reciprocal model

# Fit a reciprocal model
reciprocal_model <- lm(income ~ I(1/age), data = grad_income)

# Check poly model vs reciprocal with stargazer
stargazer(poly_model,reciprocal_model, type = "text")

# Cubic model is conslusively the best model.

# Now looking if there is a relationship in the model between age and gender

# Fit a model with interaction
interaction_model <- lm(income ~ age + gender, data = grad_income)

# cubic model with interaction
interaction_cubic_model <- lm(income ~ poly(age, 3) + gender, data = grad_income)

# Check the model with stargazer
stargazer(interaction_model, interaction_cubic_model, type = "text")

# 2) Write a few lines justifying your choice of model.

# The interaction cubic model is the best model as it has the highest adjusted R-squared value and the lowest p-value, 
# it also includes the interaction between age and gender which is important
# as the relationship between age and income is different between what type of gender the person is.

# 3) Create a graph which illustrates the relationship between age, gender, and income.

# Ggplot the interaction cubic model
grad_income %>%
  ggplot(aes(x = age, y = income, color = gender)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3),
              se = FALSE) +
  scale_color_manual(values = c("palevioletred", "cornflowerblue")) +
  # fixing the y scale values to dollars
  scale_y_continuous(labels = scales::dollar_format()) +
  # fixing the x scale for age
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Relationship between income, age and gender",
       x = "Age",
       y = "Income",
       color = "") +
  theme_minimal()+
  theme(legend.position = "bottom")



# EXERCISE 2: Non-Normal Distributions

# 1) Test whether pclass is also correlated with survival, 
# and see if fare is still significant even once pclass is included.

# Load the data
url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/titanic.csv"

titanic <- read_csv(url)

# Check the data
glimpse(titanic)

# survived and pclass are not factors but R understands 
# them as factors since they are 1 and 0 and 1,2,3 respectively

# Fit a model with pclass and fare
pclass_model <- glm(survived ~ pclass + fare, family = binomial(), data = titanic)
fare_model <- glm(survived ~ fare, family = binomial(), data = titanic)

# Check the model with stargazer
stargazer(fare_model,pclass_model, type = "text")

# Fare is correlated with survival looking at the model with only fare, 
# but when pclass is included fare is no longer significant, but pclass is.

#2) “Women and children first” was the famous policy for boarding lifeboats in shipwrecks at this time. 
# We found that women were much more likely to survive than men, but what about children?

# Creating new column for men, women and children
titanic <- titanic %>%
  mutate(
    person_type = case_when(
      age < 16                  ~ "child",
      sex == "female"          ~ "woman",
      sex == "male" | is.na(sex) ~ "man"   # Catch any NA for safety
    ),
    # make this a factor for cleaner analyses
    person_type = factor(person_type, levels = c("man", "woman", "child"))
  )

# Fit a model with person_type
person_type_model <- glm(survived ~ person_type, family = binomial(), data = titanic)

# Check the model with stargazer
stargazer(person_type_model, type = "text")

chisq.test(table(titanic$person_type, titanic$survived))

# The chi-squared test shows that there is a significant difference between
# the survival of male, female and children.

xtabs(~person_type+survived, data = titanic)

# The table shows that being a woman or a child makes a major difference to survival than being a man. 
# It also shows that being a woman is more likely to survive than being a child.

anova(pclass_model, person_type_model, test = "Chisq")

# Looking at the models we can see that the person_type model is better than the pclass model,
# as the large drop in residual deviance shows, which means the person_type model is a better fit for the data.
# We cannot see the p-value for the anova test, because the person_type model is not nested in the pclass model.

# 3) Finally, combine these factors and find the most effective model to explain the odds of surviving the Titanic

# Fit a model with person_type, pclass and fare
persontypefare <- glm(survived ~ person_type * pclass + pclass + fare, family = binomial()
                      , data = titanic)

# Check the model with stargazer
stargazer(persontypefare, type = "text")

anova(person_type_model, persontypefare, test = "Chisq")

# Seems like adding it all together plus the interaction between person_type and pclass is a better
# fit for the data, as the large drop in residual deviance shows. 
# The p-value is also significant, which means the final model is a better fit 
# for the data than the person_type model.

# Lastly I will make some new variables to see if they can improve the model even further.

# Creating a new categorical variable for family size or if the person is alone

titanic <- titanic %>%
  mutate(
    family_size = sibsp + parch,
    is_alone = ifelse(family_size == 0, "alone", "not_alone"),
    is_alone = factor(is_alone)
  )

# Creating a new variable for looking if living closer to the deck had an impact on survival
titanic <- titanic %>%
  mutate(
    deck = substr(cabin, 1, 1), # Adding na to the missing values
    deck = ifelse(is.na(deck), "no_cabin", deck),
    deck = factor(deck)
  )

# Lastly I will extract the title from the name column
titanic <- titanic %>%
  mutate(
    title = gsub("(.*, )|(\\..*)", "", name),
    title = factor(title)
  )

# Fit a model with person_type, pclass, fare, family_size, is_alone, deck and title
final_model <- glm(survived ~ person_type * pclass + fare + family_size + is_alone + deck + title,
                   family = binomial()
                   , data = titanic)

# Check the model with stargazer
stargazer(final_model, type = "text")

# looking at the categorical variables to see if the model is overfitting
table(titanic$deck, titanic$survived)
table(titanic$title, titanic$survived)
table(titanic$is_alone, titanic$survived)

anova(persontypefare, final_model, test = "Chisq")

# The model is improved but it got worse by overfitting the data as many of the new categories
# have too few observations to be significant. The p-value is also not significant,
# which means the final model is not a better fit for the data than the persontypefare model
# at the moment, I will try to group them better to see if the model improves correctly.

titanic <- titanic %>%
  mutate(deck_grouped = case_when(
    deck %in% c("G","T","F") ~ "rare_deck",
    deck %in% c("A","B","C","D","E") ~ as.character(deck),  # keep them
    deck == "no_cabin" ~ "no_cabin",
    TRUE ~ deck  # just in case
  )) 

titanic <- titanic %>%
  mutate(deck_grouped = factor(deck_grouped))

# Combining the rare titles

titanic <- titanic %>%
  mutate(title_grouped = case_when(
    title %in% c("Dr", "Rev", "Major", "Col", "Capt", "Jonkheer", "Don", "Sir") ~ "rare_title",
    title %in% c("Mme", "Ms", "Lady", "Mlle", "the Countess", "Dona") ~ "Miss",
    TRUE ~ title))

# looking at the categorical variables to see if the model is overfitting
table(titanic$deck_grouped, titanic$survived)
table(titanic$title_grouped, titanic$survived)

# Fit a model with person_type, pclass, fare, family_size, is_alone, deck_grouped and title_grouped
final_model_2 <- glm(survived ~ person_type * pclass + fare + family_size + is_alone + deck_grouped + title_grouped,
                     family = binomial()
                     , data = titanic)

# Check the model with stargazer
stargazer(final_model_2, type = "text")

anova(persontypefare, final_model_2, test = "Chisq")

# Even though the p-value is significant and the model is improved, the final model is not
# a good enough improvement to the persontypemodel. Personally I think the model
# is too complex and gets improved too little for it to be worth to devide the categories even further.
# Lastly I will remove some of the variables that are not significant and see how much
# I can remove to get a better model that is not overfitting the data.

simple_model <- glm(
  survived ~ person_type * pclass + pclass + family_size + is_alone, 
  family = binomial(link = "logit"), 
  data = titanic
)

stargazer(persontypefare, simple_model, type = "text")

anova(persontypefare, simple_model, test = "Chisq")

# 4) Write a few lines justifying your choice of this model.

# I chose the "simple model" because it is the best fit for the data, as it improves the model
# significantly while removing the term fare which didnt seem to fit the model and adds family
# size and shows if the passenger were alone, which seems to improve it also.

# The p-value is also significant, which means the simple model is a better fit for the data
# than the persontypefare model. The simple model is also not overfitting the data, as the p-values
# for the variables are significant and the model is not too complex. The simple model is also
# relatively easy to interpret and understand, which is important when presenting the results to others.
# The final_model_2 is too complex and does not improve the model 
# enough to be worth the extra complexity in my opinion.

# Link to ChatGpt:

# https://chatgpt.com/share/67698ad5-7ce0-8001-8ca1-e27d9e51654b