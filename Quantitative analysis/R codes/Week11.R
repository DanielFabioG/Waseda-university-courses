#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 11: Differences-in-Differences and Fixed Effects


# As ever, set up your R environment and your working directory; make sure you
# edit the directory below to wherever you put the files for this class.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Quantitative Analysis 2024/Week 11/")   


# We're going to be using some new packages today - I'll explain them
# as we go along.

install.packages('fixest')
install.packages('modelsummary')

# You may also need to install the Janitor package, if you haven't already:
install.packages('janitor')

library(tidyverse)


#-----------------------------------------------------------------------------#
# 1. Differences in Differences


# Let's load in some sample data to use for demonstrating fixed effects. Since 
# we talked about the example of John Snow, we can stick with the healthcare 
# theme; the file `organ_donations.csv` contains quarterly organ donation rates 
# for a number of US states between Q4 2010 and Q1 2012.

# In the middle of this period, California passed legislation which changed its
# organ donation policy from an "opt-in" approach (where you have to tick a box
# to indicate that you wish to donate your organs after death) to an "active 
# choice" approach (where you have to give either a positive or a negative 
# answer). All the other states stayed the same. 

# Kessler & Roth 2014 studied the effects of this change using a differences in
# differences modelling approach. You can read their paper here if you're 
# interested:  https://www.nber.org/papers/w20378

od <- read_csv(file = 'organ_donations.csv')
glimpse(od)


# It's possible to model differences-in-differences using the lm() command we've
# been using all along, but it's easier to use a package called `fixest`, which 
# is designed to estimate something called `Fixed Effects` in regression models.

# Differences-in-Differences is basically a special case of fixed effects. We'll 
# talk about the more general application of fixed effects later.

library(fixest)

# Annoyingly, Stargazer doesn't support displaying fixest models, but we can use
# another package called `modelsummary` instead.

library(modelsummary)

# We need to indicate in our data which of the observations are post-treatment.
# We can do this by creating a new column with a binary variable, as follows:

od <- od %>% 
  mutate(Treated = (State == 'California' & 
                      Quarter %in% c('Q32011', 'Q42011', 'Q12012')))
glimpse(od)

# This new column will be TRUE for observations of California in the last three
# quarters (post-treatment), and FALSE for all other observations.

# Now let's run the model. We use the feols() (Fixed Effects Ordinary Least Squares)
# command, which is very similar to lm() but gives us some new functionality.

# The key extra functionality is that you can add to the model a list of the 
# variables whose effects you want to fix. In our case, for a differences in
# differences model, we want to fix the effects of time and state; in other
# words, we're assuming that without the treatment, there would still be a 
# difference over time, and a difference between states (they wouldn't be identical 
# to begin with), but we're interested in knowing the extra difference created
# by the treatment.

od.did <- feols(Rate ~ Treated | State + Quarter, data = od)
msummary(od.did, stars = c('*' = .1, '**' = .05, '***' = .01))

# What is this model telling us?

# The effect of California's change was statistically significant - but it was
# negative, with organ donation rates falling 2.2% more than was seen in comparable
# states - or, in DiD terms, was 2.2% lower than California would have been expected
# to be given parallel trends.


# Long Term Effects
###################

# Remember that it's also possible to use DiD to model effects over a longer term,
# not just before-and-after. This will help us to see if our parallel trends 
# assumption is correct.

# For this, we need to tell the model that California is our state of interest,
# since that's the "treatment group".

od <- od %>% 
  mutate(California = (State == 'California'))

# This just creates a variable that is only TRUE for observations of California.

# We should also create a numeric version of our Quarter variable, because at the
# moment R thinks it's a character string, and will try to arrange it alphabetically
# since it doesn't know what order it should be in.

od <- od %>% 
  mutate(QuarterNum = case_when(Quarter == 'Q42010' ~ 1,
                                Quarter == 'Q12011' ~ 2,
                                Quarter == 'Q22011' ~ 3,
                                Quarter == 'Q32011' ~ 4,
                                Quarter == 'Q42011' ~ 5,
                                Quarter == 'Q12012' ~ 6))

# Now we can run our mode. We use the i() command inside the model specification to
# specify an interaction between two factors - in this case, we want to see
# the results for California in each of the six quarters, so we are effectively
# creating six dummy variables with this command. Note that we can use the `ref`
# parameter to specify the baseline - in this case, the last period pre-treatment.

od.did.long <- feols(Rate ~ i(QuarterNum, California, ref = 3) | State + QuarterNum, data = od)
msummary(od.did.long, stars = c('*' = .1, '**' = .05, '***' = .01))

# This summary isn't easy to read, but it shows that the coefficients for the first two 
# quarters are around zero (remember that as the baseline, the coefficient for the third
# quarter is zero), meaning that there wasn't much difference between California and the
# other states in those quarters. In the post-treatment quarters, the coefficient is
# notably larger.

# We can see this easily on a Coefficient plot:

coefplot(od.did.long)

# The proximity to zero pre-treatment, and the much more negative coefficients post-treatment,
# suggest that the differences-in-differences design is working well; California met the
# assumption of parallel trends (its trend matched other states pre-treatment), and the 
# effect of the treatment was significant and long-lasting.


#-----------------------------------------------------------------------------#
# 2. More General Cases of Fixed Effects

# For this example, let's go back to the World Health Organisation's data on
# life expectancy, which we last looked at a few weeks ago.

lifeexp <- read_csv("who_lifeexp.csv")
head(lifeexp)

# As we did last time, let's use the clean_names() function from janitor to
# automatically fix the column names, as this file includes a bunch of column
# names that include annoying things like spaces and punctuation.

lifeexp <- lifeexp %>%
  janitor::clean_names()

glimpse(lifeexp)


# Last time we looked at this data, we ended up using a multiple regression model
# to predict life expectancy based on a combination of GDP, alcohol consumption, 
# BMI, and measles cases (which we were using as a proxy for the rate of childhood
# vaccination).

fit.lm <- lm(life_expectancy ~ gdp + alcohol + bmi + measles, data = lifeexp)
msummary(fit.lm, stars = c('*' = .1, '**' = .05, '***' = .01))

# However, the data actually includes observations for the same countries for 
# multiple years - in fact, from 2000 to 2015.

# We can reasonably assume that there are some aspects of each country that are 
# unique to that country and don't vary over time much, but which have an impact 
# on the life expectancy of the population - whether those are related to physical 
# geography, culture, development stage, or any other unobserved variable.

# Therefore, let's add a country-level fixed effect to the model. First, we 
# change the country variable into a factor...

lifeexp <- lifeexp %>%
  mutate(country = factor(country))

# Then we run the model using the feols() command. As before, we add a pipe 
# operator (|) at the end of the model specification, and after it we add
# the variables we want to use for fixed effects estimation.

fit.fix <- feols(life_expectancy ~ gdp + alcohol + bmi + measles | country, data = lifeexp)
msummary(list(fit.lm, fit.fix), stars = c('*' = .1, '**' = .05, '***' = .01))

# (Quick note about the msummary() command - with stargazer, we could just add multiple
#  models to the command and it would print them side-by-side; with msummary(), we need
#  to enclose those models in a list() statement, or we'll get an error. It's a very 
#  small difference but worth mentioning so you don't get frustrated by it!)

# You can see here that adding a country-level fixed effect has actually made quite a
# significant difference. R2 is vastly improved, because a huge amount of the variance
# is country-level, and we're now controlling for that. 

# In fact, what we're now effectively seeing is something quite different. Model 2 is
# giving us a much clearer view of the effect of a CHANGE in any of our independent
# variables. It controls for the baseline of all variables in each country, so we can
# now see what happens when GDP rises, when alcohol consumption or BMI rise, or when
# the rate of childhood measles (i.e. the inverse of childhood vaccination) rises.
# The most significant change is that the sign of alcohol consumption is now reversed;
# in countries where alcohol consumption rises, life expectancy falls. 
# The sign for BMI is still positive, but the coefficient is much smaller; we've now
# controlled for the fact that BMI is higher in richer countries, so instead we're 
# likely seeing the effects of improving nourishment in developing countries.


