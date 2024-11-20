#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 7: Linear Models and Regression Analysis


# As ever, set up your R environment and your working directory; make sure you
# edit the directory below to whereever you put the files for this class.

# There are some add-on libraries for R that are very commonly used when working 
# with regression models: tidymodels and stargazer. Let's install them now (if 
# you don't have them already). We'll load them later when we need them.

rm(list=ls())
library(tidyverse)
library(tidymodels)
library(stargazer)
library(janitor)
#-----------------------------------------------------------------------------#
# 1. Linear Regression 

# The file face.csv contains data from an interesting experiment in which voters
# were asked to look at pictures of Democratic and Republican political candidates
# and say how "competent" they appear, based only on their photograph. The aim
# of the study was to see whether these perceptions helped to predict the actual
# outcomes of elections - i.e., do people partially base their vote choices just
# on an assessment of the candidate's face?

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/face.csv"
face <- read_csv(url)
head(face)

# d.comp and r.comp are the average "competency" score given to the Democratic
# and Republican candidates respectively - a value between 0 and 1.
# d.votes and r.votes are the raw vote totals in the election.

# Our first step will be to normalise this vote data. Normalising means that we 
# are going to pull every observation into the same range (often from 0 to 1), 
# so that they can be directly compared.

# Why do we need to do that here? Well, each state has a different population, so
# if we just use the raw vote totals, we'll end up with very different numbers 
# for California (the first row) compared to somewhere like Delaware (the second
# row). What we should do instead is calculate the vote share of each party as
# a percentage of the vote. We can also calculate the vote difference by just
# subtracting these values from each other - in this case, we calculate it by
# subtracting the Rep share from the Dem share, so it's the vote share difference
# for the Democratic party, and will be negative if the Democratic candidate lost.

face <- mutate(face,
               d.share = d.votes / (d.votes + r.votes),
               r.share = r.votes / (d.votes + r.votes),
               diff.share = d.share - r.share)
  
# Now we could make a simple scatter chart of the data, just like we did at the
# end of last week's class.

ggplot(data = face, mapping = aes(x = diff.share, y = d.comp)) +
  geom_point(shape = "circle filled", fill = "blue", size = 3) +
  xlab("Vote Share Difference for Democratic Party") +
  ylab("Perceived Competency of Democratic Candidate") +
  theme_minimal()


# A visual inspection of the data does suggest that there's probably a positive
# relationship. Let's try to model it with a linear regression.

# The command to run an OLS linear model in R is lm(), and we simply pass it
# the data frame we want to work on and the columns whose relationship we wish
# to model. It produces an object containing the "fitted model", which we can
# examine in various ways.
options(scipen = 999)
fit <- lm(diff.share ~ d.comp, data = face)

summary(fit)

# The summary command gives us a good overview of our model. We can see that
# the d.comp (competence) variable is highly significant (p < 0.001). The F
# Statistic (which compares this model to a version with no independent 
# variables) also has a very low p-value, so the model overall fits well.

# How should we interpret these values?

# The estimate (coefficient) for d.comp is 0.66, which implies that for a 
# change of one unit in d.comp (i.e., perceived competence going from zero
# to one), the Democratic vote share difference diff.share rises by 0.66.

# This isn't a huge effect - just 0.66% vote share change! - but of course it
# could make all the difference in a tight election. Moreover, the R2 value
# of 0.18 implies that about 18% of the variance in diff.share can be explained
# by d.comp, so perceived competency based on facial looks is actually doing 
# a lot of work in voters' choices!

# (Of course, you wouldn't want to stop there with this analysis; you'd also
# want to test potential confounding factors. For example, voters might find
# older faces to look more "competent", and also be more likely to vote for
# more experienced candidates, so you should include age as a variable, and
# there could be a similar effect for gender.)


# There are a few more functions we can use to examine the fitted model.

# coef() just gives us an object with the coefficients, which is useful if
# you want to use those as an input into another calculation.

coef(fit)

# fitted() shows you the predicted values for Y according to this fitted
# model. (I'm using head() here to just show the first few values, since 
# this will provide a value for every row of data.)

fitted(fit) %>% head()


# augment() is a command from the tidymodels package which returns the 
# original data used to create the model, but with all of the estimated
# values from the fitted model added - this is useful if you want to 
# directly look at the residuals, for example.

augment(fit)  %>% head()


# Finally, if you want to output a table of this regression for use in
# a publication, the stargazer library provides a powerful function for
# doing that.


stargazer(fit, type = "text")

# You can also tell stargazer to output the table to a file as well as
# putting it in the output window directly:

stargazer(fit, type = "text", out = "face_model.txt")

# There are lots of options you can set in Stargazer (we'll see some
# more of them in future classes), but one of the important ones is
# the ability to specify something different for the type parameter.

# If you want to import tables into Word (or put them on the web), you
# should tell stargazer to export the table as HTML; Word can understand
# HTML files and will import this as an editable table.

stargazer(fit, type = "html", out = "face_model.html")

# (The other option is to output as "latex", which is a very powerful but
# very complicated markup language for laying out professional-looking
# documents. If you know how to use LaTeX, this will be useful for you.)


#-----------------------------------------------------------------------------#
# 2. Linear Regression Graphs

# Now that we've done a linear regression, how can we include its results in
# our graph? 

# We could access all the data from the fitted model and use the coefficients to
# draw a line on top of the data - but in ggplot2, it's actually even more simple,
# as we can simply tell the software that we want a graph line based on the linear
# model function lm().

ggplot(data = face, mapping = aes(x = diff.share, y = d.comp)) +
  geom_point(shape = "circle filled", fill = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  xlab("Vote Share Difference for Democratic Party") +
  ylab("Perceived Competency of Democratic Candidate") +
  theme_minimal()

# All I've added here is one extra command to our previous scatter plot,
# geom_smooth(), which I tell to use the "lm" (linear model) method.

# Note that it has automatically included a shaded area showing the standard
# errors for the regression line. This is often very useful, as it gives the 
# viewer some idea of the confidence of the prediction, but there may be some 
# cases where you don't want it; you can disable it by setting se = FALSE.

ggplot(data = face, mapping = aes(x = diff.share, y = d.comp)) +
  geom_point(shape = "circle filled", fill = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Vote Share Difference for Democratic Party") +
  ylab("Perceived Competency of Democratic Candidate") +
  ggtitle("Perceived Facial Competence and Vote Share") +
  theme_minimal()


#-----------------------------------------------------------------------------#
# 3. Multiple Regression

# The file who_lifeexp.csv contains data from the World Health Organisation 
# with a number of indicators about life expectancy, infant mortality, and 
# national health systems.
url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/who_lifeexp.csv"
lifeexp <- read_csv(url)
head(lifeexp)

# First things first - as often happens, this data has a bunch of column
# names that would be annoying for us to use in our code, as they have
# spaces and other punctuation characters in them.

colnames(lifeexp)

# The Janitor package has a handy function called clean_names() which 
# automatically converts tricky names into an easier format. It's very 
# "opinionated", in the sense that the authors of tidyverse have strong 
# ideas about the correct format for variable names, but that's generally fine.


# janitor hack for fixing names
lifeexp <- lifeexp %>%
  clean_names()

colnames(lifeexp)


# Okay. Let's say we want to predict life_expectancy. Which variables
# might we hypothesise are connected to it?

# We might reasonably expect it to be impacted by GDP (as we previously
# saw, GDP does have a positive correlation with life expectancy, but
# it's somewhat complex - not a straight line). Other factors which
# might play a role include alcohol consumption, BMI, and percentage
# of expenditure of per-capita GDP on healthcare

# We can model all of these relationships in a single multiple regression.

multifit <- lm(life_expectancy ~ gdp + alcohol + bmi + percentage_expenditure, data = lifeexp)

summary(multifit)
stargazer(multifit, type = "text")

# These results suggest that we have found some quite good variables
# to explain life expectancy - but maybe not in the way we had expected!
# GDP is positively correlated with life expectancy, as you would 
# imagine - but higher alcohol consumption and higher BMI are also
# correlated with higher life expectancy.

# This is likely due to some degree of multicollinearity, because
# both BMI and alcohol consumption are generally higher in wealthy 
# countries.

# Meanwhile, the percentage of expenditure on healthcare is not
# seen to be statistically significant, which is also quite
# surprising. Is there another variable we might use which would
# give us a different measurement of the quality of the country's
# healthcare system?

# One possibility is the availability of vaccines - how widespread the
# infant vaccination program is in particular. It's hard to create a 
# variable that perfectly reflects vaccine availability, but let's say 
# that the number of reported measles cases per 100,000 is a good indicator, 
# since this should be proportional to vaccine coverage.

multifit2 <- lm(life_expectancy ~ gdp + alcohol + bmi + measles, data = lifeexp)

summary(multifit2)
stargazer(multifit2, type = "text")

# We do find a very strong correlation for the measles variable, which 
# suggests that this may be a good proxy to use for healthcare quality.

# By the way, Stargazer lets us include multiple different models in 
# a single table, which is really useful for quickly comparing them
# side by side.

stargazer(multifit, multifit2, type = "text")


