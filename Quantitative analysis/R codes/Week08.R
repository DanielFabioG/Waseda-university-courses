#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 8: Regression Analysis (2)
options(scipen=999)
rm(list=ls())
library(tidyverse)
library(stargazer)

#-----------------------------------------------------------------------------#
# 1. Regression with Categorical Variables


# Let's start by loading in some data with a number of categorical variables.
# The "prof_salaries.csv" file contains about 400 observations of data about 
# salaries for university professors in California.

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/prof_salaries.csv"
salaries <- read_csv(file = url)

# Let's take a quick look at the column specifications...

spec(salaries)

glimpse(salaries)

# We can see three continuous variables here - yrs.since.phd, yrs.service,
# and the salary variable (which we'll be using as our outcome variable).

# In addition, there are three variables encoded as characters - rank (which
# is the job position the person holds), discipline (the field they study), and
# sex (their gender).


# At the moment, each of those columns is just a whole bunch of strings of text.
# However, we know that those strings are actually category names, and we want
# R to treat them as such. In many cases, R will do this automatically, but to
# ensure everything is going as we intend, it's good practice to directly tell
# R that we want these columns to be categories - which R calls "factors".
# We can do this with the factor() command.

factor(salaries$rank)

# You can see here that factor() has given us the category values for each entry,
# plus a list at the end of the "levels" - i.e. the unique categories included in
# this variable.

# Let's use this command to convert all of the categorical columns in the data frame
# into proper factors.

salaries <- salaries %>%
  mutate(rank = factor(rank),
         discipline = factor(discipline),
         sex = factor(sex))

glimpse(salaries)

# You can see that rank, discipline, and sex are now marked as <fct> for factor,
# while previously they would have been <chr> for character. Now that they are 
# factors, there are a few useful things we can do. For example, let's check
# how many instances of each category in the "rank" variable are found.

salaries %>%
  count(rank)

# We can also represent this data easily as a bar chart, of course.

ggplot(data = salaries, mapping = aes(rank)) +
  geom_bar() +
  theme_minimal()


# 1.1 Binary Categorical Variables
#---------------------------------

# We have two binary categorical variables in this data - sex, which has the
# levels "Male" and "Female", and discipline, which has the levels "A" and "B".

# Let's see what happens if we do a simple regression where we use the sex of
# the individual as the independent variable, and their salary as the outcome.

fit <- lm(salary ~ sex, data = salaries)
stargazer(fit, type = "text")

# You can see here that R has automatically converted the sex variable into a
# dummy variable for us - we don't need to do anything special here, since
# it's pretty obvious to R that a regression with a factor variable should be
# handled this way.

# Interpreting the results might be slightly tricky, though. The new dummy variable
# is called "sexMale", so R has coded Male as "1" in this analysis (and consequently
# Female as "0"). Remember that the coefficient can be interpreted as the change
# in the outcome variable for a single unit change in the independent variable; in
# this case, a single unit change (from 0 to 1) is the difference between the subject
# being female and the subject being male. In other words, the coefficient value
# ($14,088) is the expected difference in salaries between women and men, and the very
# low p-value (***p<0.01) means the difference is statistically significant.

# Similarly, we can interpret the intercept (Constant) in a useful way here. The
# intercept is the value of the outcome variable when the independent variable is
# zero, and in this case a value of 0 for the independent variable means that the
# subject is female. In other words, the average income for female subjects in this
# model is $101,002 (the value at the intercept), and we can therefore easily
# calculate the average income for male subjects as well - the value of the outcome
# variable at x = 1 will be the intercept plus the coefficient, so:
#   $101,002.40 + $14,088.01 = $115,090.41


# We may of course want to change the way those factors are ordered - perhaps you
# have some reason to want Male to be coded as 0 and Female as 1 (for example, to
# maintain consistency between different parts of an analysis project). To do this,
# we can use the fct_relevel() function, which moves the specified categories to
# the top of the factor list (i.e. puts them first in the ordering).

salaries <- salaries %>%
  mutate(sex = fct_relevel(sex, "Male"))
 
# We can check that this has done what we intended with the contrasts() function:
contrasts(salaries$sex)

# This shows us that Male is now the default (i.e. 0), and Female is now coded as 1.

fit <- lm(salary ~ sex, data = salaries)
stargazer(fit, type = "text")

# Rerunning the analysis shows us the same result, but we now have a coefficient for
# sexFemale instead of sexMale, and unsurprisingly it is precisely the negative of 
# the previous coefficient.


# We can also, if you want, plot a chart of these results... although it arguably
# looks a little bit odd.

ggplot(data = salaries, mapping = aes(x = sex, y = salary)) +
  geom_point() +
  geom_smooth(mapping = aes(x = as.numeric(sex), y = salary), method = "lm", color = "red") +
  xlab("Gender") +
  ylab("Salary") +
  theme_minimal()

# Note that we had to define the mapping again for the geom_smooth() function that
# adds the regression line. This is because the ggplot function doesn't really
# understand what to do with the factorial data here; by explicitly using the
# as.numeric function we tell it to calculate the regression line with 0 and 1
# values, rather than factors.


# 1.2 Multi-Category Variables
#-----------------------------

# Now we've seen how to work with binary categories, but what about multi-category
# variables? In this data set, we have one such variable:

levels(salaries$rank)

# Once again, let's see what R does with this factor variable without any extra
# input from us.

fit <- lm(salary ~ rank, data = salaries)
stargazer(fit, type = "text")

# Here we have ended up with two dummy variables - rankAsstProf and rankProf - 
# each with their own coefficient and p-value. They're both significant at
# the p < 0.01 level, but how can we interpret these coefficients?

# Note that just as there was no dummy variable for one of the genders in the
# previous example, here we're lacking a dummy for AssocProf. This is because
# R has chosen this category as the "default", and the values we are seeing
# in the coefficients are therefore the differences between each of these
# categories and the default category. In other words, Assistant Professors
# earn $13,100 less than Associate Professors, while full Professors earn
# $32,895 more. We can also interpret the Constant similarly, as meaning that
# the average earnings for an Associate Professor are $93,876.

# In this case, we might want to change the order of the categories again;
# it would be easier to read if we compared all of the other categories to
# the one with the lowest average income. We could do that by manually
# checking the averages and using fct_relevel() as we did before, but there
# is another useful function which will do this for us automatically:

salaries <- salaries %>%
  mutate(rank = fct_reorder(rank, salary))

# fct_reorder() will reorder the factor variable (rank) according to the
# values of another variable (salary). Specifically, it will rank them
# according to the median value of salary, from low to high - you could
# of course change this by adding extra parameters to the command.

fit <- lm(salary ~ rank, data = salaries)
stargazer(fit, type = "text")

# Here we can see that this has accomplished what we wanted; AsstProf is
# now the default level, so we get to see the increase in salary from being
# promoted to AssocProf and Prof.


# 1.3 Putting it Together
#------------------------

# Before we move on, let's see how we would combine multiple categorical 
# variables into a more complex multiple regression.

# In this case, we might be interested to know whether gender has an effect
# on salary independently of its effect on rank. In other words, are women
# being paid less regardless of their rank, or is their average lower salary
# a result of fewer women being promoted to high-ranking positions?

fit <- lm(salary ~ sex + rank, data = salaries)
stargazer(fit, type = "text")

# Remember that we have to interpret these results ceteris paribus - the 
# coefficients are based on all other parts of the model being held equal.
# Interestingly, in this case, we can see that the sexFemale dummy variable
# is no longer statistically significant. This strongly suggests that
# the difference in pay between genders in this data is due to different
# promotion opportunities, rather than men being paid more in the same 
# roles - which makes sense in a university, as many universities have 
# public salary scales, so a gendered pay gap in equivalent roles would be 
# very obvious and highly criticised. Instead such a gap is much more likely
# to emerge due to the (harder to prove) phenomenon of women being passed 
# over for promotion opportunities.


#-----------------------------------------------------------------------------#
# 2. Regression with Interacting Variables


# Let's use the salaries data set again, and this time take a look at the
# possibility that there is an interaction between sex and yrs.service on the
# salary being paid. Our hypothesis is that the number of years a professor has
# worked is considered differently depending on their gender. Let's add that 
# interaction to a linear model.


# In R, we define an interaction using the colon (:) operator between two variable
# names in the model specification. Note that you should also include the interacting
# variables separately in the model - this is due to a rule called the Hierarchical
# Principle, which states that when you include an interaction effect in a model, the
# main effects of the interacting variables should also be included, even if those
# main effects don't turn out to be statistically significant.

fit.1 <- lm(salary ~ yrs.service + sex, data = salaries)
fit.2 <- lm(salary ~ yrs.service + sex + yrs.service:sex, data = salaries)
stargazer(fit.1, fit.2, type = 'text')

# R also gives us a shortcut here in the form of the * operator, which creates
# an interaction effect and automatically includes both variables' main effects
# in the model as well. You can use either : or * freely; they both do the
# same thing, so it's your personal preference which of them is easier to read.

fit.2 <- lm(salary ~ yrs.service * sex, data = salaries)
stargazer(fit.1, fit.2, type = 'text')

# These results tell us that there is quite probably a relevant interaction in
# this data. The interaction term is statistically significant (albeit only at
# a low level, p<0.1) and the Adjusted R2 rises from 0.115 to 0.120. These are
# marginal effects, but it would still be worth exploring some more to find
# out if the gender effect is relevant to the analysis.


# Note that we cannot easily interpret the coefficients of models that include
# interaction effects. In this case, the model with the interaction effect
# is a little better at prediction (Adjusted R2 rose slightly), so it's certainly
# a better model - but the coefficients for yrs.service and sexFemale are no 
# longer meaningful on their own. 


# To understand what's actually happening with an interaction term in a model,
# it's often helpful to create a graph showing how the variables in question are 
# related to the outcome variable. 

ggplot(data = salaries, mapping = aes(x = yrs.service, y = salary, color = sex)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm") +
  theme_minimal()


# What we're doing here is plotting the relationship between years of service and
# salary, divided up according to gender. By telling ggplot2 to map "color" to the
# sex variable, it automatically knows that we want to visualise three variables
# in this way.

# If gender had no impact on this relationship, these lines would be parallel. Instead, 
# they actually cross, although they both still have positive slopes. This means
# that we're observing an exponential interaction - years of service has a positive
# effect on salary regardless of gender, but gender does change the strength of that
# positive relationship.




