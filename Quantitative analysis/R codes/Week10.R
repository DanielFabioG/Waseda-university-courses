#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 10: Advanced Regression Topics (2)


# As ever, set up your R environment and your working directory; make sure you
# edit the directory below to wherever you put the files for this class.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Quantitative Analysis 2024/Week 10/")   


library(tidyverse)
library(stargazer)

#-----------------------------------------------------------------------------#
# 1. Comparing Regression Results

# Let's load in the data you were looking at in the homework for Week 8, not forgetting
# to convert the `country` column into a factor.
url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/pol_survey.csv"
survey <- read_csv(url)
survey <- survey %>%
  mutate(country = factor(country))

# Now let's run four models to test the predictors of responses to the Alison vignette.
# We want to test for possible interactions between `self` and both `age` and `country`.
# We'll do one model with no interactions as a baseline, then test each interaction
# separately, and finally do a model with both interactions.

fit1 <- lm(alison ~ self + country + age, data = survey)
fit2 <- lm(alison ~ self + country + age + self:age, data = survey)
fit3 <- lm(alison ~ self + country + age + self:country, data = survey)
fit4 <- lm(alison ~ self + country + age + self:age + self:country, data = survey)
stargazer(fit1, fit2, fit3, fit4, type = "text",
          column.labels = c("None", "Self:Age", "Self:Country", "Both"))


########################
# Picking the best model

# First, let's look at the "goodness of fit" statistics.

# The p-values of the F-statistic for all four models are low, so they are all valid
# models.
# The highest Adjusted R2 is 0.283, for the Self:Country and Both models. 
# Similarly, both of those models have the lowest residual standard error, 1.003.

# We should choose either the Self:Country and Both model for our data. But which 
# one is better?

# There is a strong argument that Self:Country should be the chosen model, even though
# the Both model has exactly the same goodness of fit statistics. This is for two reasons.

# 1) The Self:Age interaction is not statistically significant, so it's hard to justify
#    its inclusion in the model.

# 2) The Self:Country model has one less term than the Both model - in other words, it
#    achieves the same goodness of fit with a simpler model. In this instance, the
#    principle of parsimony (or Occam's Razor, or the KISS principle - whatever you
#    prefer to call it) says we should opt for the simpler model.


# Now let's use ANOVA to compare these models.

# First, let's see if adding the Self:Country interaction term (fit3) produces a statistically
# significant improvement in the model, by comparing it to the model with no interaction terms
# at all (fit1).

anova(fit1, fit3)

# The results show us that there is a statistically significant (at the 0.05 level) improvement
# from adding this interaction term.

# How about the difference between Self:Country (fit3) and Both (fit4)?

anova(fit3, fit4)

# This time, there is no statistical significance. Adding an extra term (self:age) to the model
# has made no significant improvement to its fit. Therefore, there is no reason to add
# complexity to the fit3 model through the inclusion of this extra term.


#-----------------------------------------------------------------------------#
# 2. Logistic (Logit) Regression


# To demonstrate a logistic regression, we're going to use a very famous data set - the passenger
# list from the Titanic, including details of whether the passenger survived the sinking of
# the ship or not.

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/titanic.csv"
titanic <- read_csv(url)

glimpse(titanic)

# For this example, we're going to check whether the fare that people paid is related to their
# chances of survival. Our hypothesis is that you were more likely to survive if you paid more
# for your ticket.

# The `survived` variable is of course a binary variable - it's either 1 (survived) or 0 (did not
# survive). There are no other possible values. Consequently, to predict this outcome we need
# to use a logistic regression model.

# (Side-note: it is possible to use a standard OLS linear model for this kind of binary data
# as well; this is called a Linear Probability Model (LPM) and in some cases it is a very
# useful tool for showing how a continuous input variable impacts the probaility of a 
# binary outcome occurring. However, it has problems too - the most obvious being that
# it might predict an outcome lower than 0, or higher than 1 - so should be used with care.)

# In R, we do this using the glm() function. Up until now we have been using lm() which is the
# function for standard linear models. glm() means "generalized linear model", which means it
# can be used in situations where the outcome variable is not continuous data (i.e., a quantity)
# but is instead a category of some kind.

titanic.fit1 <- glm(survived ~ fare, family = binomial(), data = titanic)
stargazer(titanic.fit1, type = "text")

# Our hypothesis seems correct - paying more for your fare did increase your chances of 
# surviving the disaster, and this value is statistically significant.

# How can we interpret this coefficient?
# Logistic regression produces log odds coefficients, not probabilities like OLS does. We can 
# convert these back into standard odds, which are easier to interpret, with the exp()
# function (exponent being the reverse of logarithm):

exp(coef(titanic.fit1))

# What this means is that for every 1-unit increase in fare paid (£1, which was quite a lot
# of money in those days), passengers' odds of survival were multiplied by 1.036 - meaning
# a small but significant increase.

# We can also see the 95% confidence ratio for these values:

exp(confint(titanic.fit1))

# The confidence interval is between 1.026 and 1.047 - it does not include 1, which means
# that the result is statistically significant. Remember that the null hypothesis is that
# `fare` has no effect on `survival`, which would mean odds of 1 - this is different from
# when we're using a standard coefficient, when no effect is 0! In this case, if 1 is
# within the 95% confidence interval, we cannot reject the null hypothesis.

# By the way, we can also plot the logistic model, although this tends to be less useful
# than with a linear model plot.

ggplot(data = titanic, mapping = aes(x = fare, y = survived)) + 
  geom_point(alpha = 0.4) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme_classic()


# We can of course also include more complex factors in a logistic model. Let's add gender
# to the mix - but note that we need to turn it into a factor first, of course.

titanic <- titanic %>%
  mutate(sex = factor(sex))

titanic.fit2 <- glm(survived ~ fare + sex , family = binomial(), data = titanic)
stargazer(titanic.fit1, titanic.fit2, type = "text")


# We can see here that gender also has a notable effect on the chances of survival.
# Fare remains statistically significant in the new model, but gender is also
# significant at the 0.01 level.

exp(coef(titanic.fit2))

# Being male instead of female multiplies the chance of survival by 0.083, suggesting
# a pretty huge gender disparity in the survival results. Does that seem right?

# Let's look at the raw numbers in a cross table.

xtabs(~sex+survived, data = titanic)

# Out of 649 men on the ship, 133 survived - that's about 20%, but the odds are 133/516 (0.257).
# Out of 388 women on the ship, 292 survived - about 75%, with odds of 292/96 (3.041).

# So yes - the odds of survival as a woman were more than ten times higher than the odds of
# survival as a man.

###########################
# Comparing Logistic Models

# You might notice that we end up with quite different goodness of fit statistics when we
# run a logistic model. The most important one to note is the Log Likelihood of the model,
# which we can use to compare models and see which is doing the best job of describing the
# data.

# Log Likelihood doesn't mean very much on its own (there's no "good" or "bad" value for
# this number), but when you're comparing models using the same data set, you want the
# Log Likelihood to be as high as possible. Here, we can see that the first model has a
# Log Likelihood of -671, and the second has -521 - so the second model is a better fit.

# We can also use ANOVA to compare these models, just like OLS models - but in this case,
# we want it to use the Chi Squared test, since we're dealing with a binary (categorical)
# outcome variable. We can specify that as follows:

anova(titanic.fit1, titanic.fit2, test = "Chisq")

# Just like in the OLS models, this is confirming that model 2 is a statistically significant
# improvement over model 1 in terms of fitting the data.


#-----------------------------------------------------------------------------#
# 3. Poisson Regression

# Poisson Regression is often used for situations where the outcome variable is a count
# of the number of times something has occurred, in which the odds of the event 
# occurring are not dependent on previous occurrences. This creates what is called
# the Poisson Distribution - as this is not a normal distribution, we need a different
# regression model for this data.

awards <- read_csv('student_awards.csv')
glimpse(awards)

# This data shows the number of awards won by high school students, and their score in 
# maths in the current year. Most students win zero awards; the maximum in the data is 6.

# One of the column names has a space in it, which is annoying - let's fix that.

colnames(awards) <- c('Awards', 'MathScore')


# Okay, now we're going to fit two models - a standard OLS linear model (the family
# parameter for this is `gaussian()`, which is another name for the normal distribution)
# and a Poisson regression model. Once again we're using the glm() function.

award.fit1g <- glm(Awards ~ MathScore, family = gaussian(), data = awards)
award.fit1p <- glm(Awards ~ MathScore, family = poisson(), data = awards)
stargazer(award.fit1g, award.fit1p, type = 'text')

# You can see that the Poisson model has a better fit (significantly higher log likelihood), 
# although both models do give statistically significant findings for MathScore. 

# We can also plot this outcome, which helps to show why Poisson is a better fit for the
# data - which is very clearly not normally distributed.

ggplot(data = awards, mapping = aes(x = MathScore, y = Awards)) + 
  geom_point(alpha = 0.4) + 
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  theme_classic()

# And once again, just as with logistic regression, the coefficients from Poisson Regression
# are in log odds format:

coef(award.fit1p)

# So we use exp() to turn them into actual odds...

exp(coef(award.fit1p))

# For every 1 that MathScore increases, the chances of receiving an award are multiplied by
# 1.079 - a small but significant increase.


#---END---
