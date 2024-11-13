#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 6: More Statistical Tests: ANOVA, Chi-Square, Correlation etc.
rm(list = ls())
library(tidyverse)


#-----------------------------------------------------------------------------#
# 1. ANOVA (ANalysis Of VAriance)

# Let's start by loading in some data which has sampled observations from several
# different groups - in other words, data where the predictor variable (the group
# to which the observation belongs) has more than two possible values.

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/grad_income.csv"
grad_income <- read_csv(file = url)

head(grad_income)

# This dataset has five columns, each holding a sample of thirty annual incomes
# for graduates from different major Tokyo universities (sums are in million Yen).
# Our question is: does the school make a significant difference to the income?

# Before we start, we need to change the format of the data a little bit. This 
# "wide" table - with the different groups in individual columns - is common, but
# not really appropriate for statistical work, because most software will assume
# that each row is an observation, which in this case isn't true. To fix this, we
# use the pivot_longer() function to spread the observations out into a long list.

grad_income.long <- grad_income %>%
  pivot_longer(c(Todai, Waseda, Keio, Meiji, Sophia), names_to = "university", values_to = "income")

head(grad_income.long)

# You can see that we have ended up with two columns - one row per observations,
# listing the university and the income.


# Before we run ANOVA, let's have a look at our data. Remember, creating graphs
# for your own interpretation while you are working on any quantitative problem
# is a key way to avoid mistakes or unexpected results during the process!

ggplot(data = grad_income.long, mapping = aes(y = income, x = university)) +
  geom_boxplot()


# We can see that there's a significant difference among the school samples, but
# also a lot of overlap. Could the differences be statistically meaningful?

# Let's run ANOVA and find out. The function to do this in R is aov()
# Note that we can run these statistical commands inside a tidyverse piped
# command chain, by using the period (.) to indicate where the piped data
# should go in the command.

grad_income.long %>%
  aov(income ~ university, data = .) %>%
  summary() 

# This result is relatively easy to interpret. Just as with the t-test, the
# degrees of freedom (df) is the number of variables, minus 1; the residuals 
# are the the number of observations (n) minus the number of variables. 
# The sum and mean of squares on the top row are the between-group measurements;
# on the bottom row are the within-group measurements. The F-value is just
# these figures divided into each other, and effectively measures how much
# variance is being explained by the groupings.

# The important statistic, like with the t-test, is the P value. In this
# case, p is high - well above the 0.05 level, so we have to conclude that 
# despite the differences we can see in the five samples, these differences
# could have arisen randomly from sampling. In other words, we can't reject
# the null hypothesis that all the samples have the same means; there is no
# statistically significant difference between them.


# Let's try this again with a more complex data sample - this time taken from
# the real world. We'll use the package "gapminder"; it contains a useful data
# set of world population data that we can use to try out statistical tests
# on real-world data.

require("gapminder")

# Here's a quick look at the data set it builds into R for us.
head(gapminder)

# Let's construct a data set from this. I want to look at life expectancy
# figures across three continents and see if there's a significant difference
# between the continents. I'll pick 2002 as the year to examine.

life_exp <- gapminder %>%
  filter(year == 2002 & continent %in% c("Asia", "Europe", "Americas")) %>%
  select(continent, lifeExp)

head(life_exp)

# You can see that this is the same kind of table we were using for the income
# data - one row per observation. Note that we've dropped the country names;
# for our purposes, each country is just one observation of its continent.

ggplot(data = life_exp, mapping = aes(y = lifeExp, x = continent)) +
  geom_boxplot()

# Again, we can see that there's a lot of variation between groups, but also
# significant overlap. Note that both Asia and the Americas have one outlier
# down at the bottom of the graph, indicated by a dot - this can be interpreted
# as a case that would be an outlier (beyond the 95% range) in a normally
# distributed sample.
options(scipen = 999)
life_exp %>%
  aov(lifeExp ~ continent, data = .) %>%
  summary() 

# This time, we have found a significant result - the P value is far below
# 0.05, and in fact is significant even at the 0.001 level (99.9% confidence).
# Note that the summary() command helpfully puts stars after significant 
# results to indicate which confidence level they exceed; this isn't too
# useful here, with one result, but these stars will be very helpful for
# quickly assessing the results of more complex statistical functions.


# The above ANOVA test confirms that at least one of these samples has a 
# statistically significant difference from another of the samples.
# However, a downside of ANOVA is that it doesn't tell us which sample
# that is! To get more information about our samples, we can pass the 
# ANOVA results to a function called TukeyHSD() - Tukey's Honestly Significant
# Difference test - which will test all of the possible pairs within
# our data to find which of them have significant differences.

life_exp %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD()

# Looking at the P values, we can see that there is no significant difference
# between Asia and the Americas, but the differences between Europe and the 
# other continents are significant. 

# Usefully, we can plot these results in a simple graph - this is especially
# helpful if you're dealing with a lot of groups and want to see the results in a
# quick summary.

life_exp %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD() %>%
  plot()

# This graph shows the 95% confidence intervals for the three pairings in this
# data set. You can see that the Asia-Americas pairing has a confidence interval
# which includes zero - i.e. the null hypothesis - while the other two pairings
# reject the null hypothesis, which lies outside their confidence intervals.



#-----------------------------------------------------------------------------#
# 2. Chi-Square Test

# Let's load in some categorical data.
url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/vote_income.csv"
vote_income <- read_csv(file = url)
head(vote_income)

# This file has a list of observations of voting preference according to income
# bracket. Both of these are categorical variables; voting preference has three
# levels (Ishin, CDPJ, and LDP), as does income (Low, Medium, and High).

# For this kind of data, we can very quickly generate a CONTINGENCY TABLE that
# will show us the number of observations for each category combination.

table(vote_income)

# Already it looks like there's quite a difference in voting preferences depending
# on income levels, but let's test that hypothesis with the chi-square test.

# GOODNESS OF FIT
# ---------------

# Firstly, we should test to see if our data fits the expectation that there are
# an equal number of Low, Medium, and High income earners in the population (e.g.
# where we had set those brackets according to 33rd and 66th percentiles). Given
# this assumption, how likely is it that we would have taken a random sample with
# these proportions in it?

table(vote_income$income)

# Here are the counts for just the income levels. We can see there's quite a large
# difference; could this have arisen randomly in the sampling process?

vote_income %>%
  select(income) %>%
  table() %>%
  chisq.test()

# Here we can see that the P value is very small - well below 0.05, or even 0.001.
# This means that we can reject the null hypothesis, which was that the proportion
# of Low, Medium, and High income earners was equal - this sample is extremely
# unlikely to have come from a population with equal proportions of those 
# categories.

# Note that we don't run this test on the voting preferences, since there is no
# reasonable expectation (in most cases) that we'll find an equal proportion of
# voting preferences in any given sample.


# TEST OF INDEPENDENCE
# --------------------

# Now we can move on to testing whether the two variables' distributions are
# independent of each other. 

# Here, the null hypothesis is simply that there is no relationship between these 
# variables. Rejecting that null hypothesis means that one of these variables is 
# probably influencing the other - in other words, knowing the value of one
# variable gives us information about the probable value of the other variable.

vote_income %>%
  table() %>%
  chisq.test()

# Here we can see that P is extremely low again; we can reject the null 
# hypothesis that these variables are independent of each other. 


# EXPECTED VALUES & FISHER'S EXACT TEST
# -------------------------------------

# The Chi-Square test is very useful, but it is a parametric test - this means
# that it makes certain assumptions about the distribution of the variables
# in the data. When you are dealing with data that probably doesn't match 
# those expectations, or data in which the expected values (i.e. the values
# the algorithm thinks it should find given a certain distribution) are very
# small, it's best to double-check your Chi-Square results using another,
# nonparametric test, called Fisher's Exact Test.

# How do we find out what the expected values in a Chi-Square test are?

vote_income %>%
  table() %>%
  chisq.test() %>%
  .$expected

# In this case, all of our expected values are over 5, which is the usual 
# cut-off point for needing to use Fisher's Exact Test. In a real research
# project, it would be fine just to report Chi-Square results and move on.

# For the sake of completeness, let's look at the code to run a Fisher's 
# Exact Test on this data.

vote_income %>%
  table() %>%
  fisher.test()

# You can see that it's almost exactly the same as above - we just change the
# name of the test function. It still uses the same contingency table data
# format as the Chi-Square test.

# In this case, it also gives almost exactly the same result, which is
# unsurprising, since in cases where all the expected values are above 5,
# Fisher's Exact Test functions almost exactly the same as Chi-Square.


#-----------------------------------------------------------------------------#
# 3. Correlation - Pearson's R

# For some example data for correlation, let's go back to the gapminder data 
# set, since this is real demographic data.

head(gapminder)

# We might expect that there is a clear connection between GDP Per Capita and
# Life Expectancy - it stands to reason that wealthier countries should have
# higher life expectancy in general. Let's extract those figures from the
# data and find out.

gdp_life <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, gdpPercap)

# The simplest way to visualise this kind of data is as a scatter plot.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# We can see that there does seem to be a positive correlation, but
# it's not necessarily a simple one. A "perfect" correlation would
# be a straight line at 45 degrees up through the graph; in this case
# it's probably more of a curve. That's important for the type of test
# we choose to use, as we'll see in a moment.


# To perform the Pearson's R test, we simply use the function cor.test()

gdp_life %>%
  cor.test(~ lifeExp + gdpPercap, data = ., method = "pearson")

# It's worth taking a moment to look at that formula. Up until now, we
# have been writing formulae like "x ~ y", meaning "x is dependent on y";
# here, we're writing "~ x + y", which means "the result (R) is dependent
# on both x and y". This is a little different from regression, which
# we'll start looking at next week; just bear in mind that when doing a
# correlation, the formula is different.

# The cor.test() function gives us the correlation coeffecient R, 0.68.
# This coeffecient is always between -1 and +1; values closer to zero
# indicate a weak correlation, while at the extremes they indicate 
# strong negative or positive correlations.

# R > 0.7 is generally taken to be a strong correlation; anything over
# 0.5 is moderately strong. However, R on its own does not tell us if
# the correlation is statistically significant - so the test formula
# also reports a P value. In this case, the P value is very low, so
# we can assume that the correlation is positive, moderately strong,
# and statistically significant.


# By the way, you can easily add these values to your scatterplot
# using the stat_cor() function from the ggpubr library:

ggplot(data = gdp_life, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  ggpubr::stat_cor(method = "pearson")


# We can also add the correlation line to this graph. (Technically, we're
# adding a simple regression line, but we'll get to that next week.)

ggplot(data = gdp_life, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggpubr::stat_cor(method = "pearson")


# Incidentally, if we look at the histogram of this data, using the same 
# command to generate a graph we used last week...

ggplot(data = gdp_life, mapping = aes(x = gdpPercap, y = ..density..)) +
  geom_histogram() 

# ... We can see that this graph hints at there being non-normal
# distributions for this data - at the very least, they're heavily weighted
# towards one side or the other. This explains why the graph has a curve
# in it, rather than being a straight relationship between values.

# For this kind of data, Spearman's rho is a better test. It is a
# nonparametric test; this means that it doesn't assume a normal 
# distribution, and it also doesn't assume a linear relationship
# between the values. 

gdp_life %>%
  cor.test(~ lifeExp + gdpPercap, data = ., method = "spearman")

# The only thing we have to change is the method parameter. Here we see
# that rho is reported as 0.86 - much higher than R which was 0.68. This
# is because the Spearman test is better at dealing with the non-linear,
# curved nature of this data.

# Note however that even though rho is dealing more effectively with the 
# curved data, it is still only generating a simple correlation
# coefficient. If you plotted this line on a graph it would still be straight.

# To handle this kind of curved data better, we're going to have to move to 
# regression testing - which is what we'll do next week.
