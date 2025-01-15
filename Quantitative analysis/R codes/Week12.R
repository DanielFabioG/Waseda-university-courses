#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 12: Time Series Analysis


# As ever, set up your R environment and your working directory; make sure you
# edit the directory below to wherever you put the files for this class.

rm(list = ls())

library(tidyverse)

#-----------------------------------------------------------------------------#
# 1. Working with Time Series Data

# Let's load in some time series data. The file retail_sales.csv contains
# retail sales data over a long period of time; each entry contains a day's
# date and an inflation-adjusted sales number.

url <- "https://raw.githubusercontent.com/DanielFabioG/data/refs/heads/main/retail_sales.csv"
retail <- read_csv(url)
glimpse(retail)

colnames(retail) <- c('DATE', 'Sales')

# First, we need to tell R that this is a time series - at the moment, it's just
# viewing those dates as character strings. 

# There is a built-in function ts() for this, but it's very inflexible. The
# `feasts` and `tsibble` packages provide a much better function. 

library(tsibble)
library(feasts)

# Before we turn our data frame into a time series, we need to tell R how to convert
# the character strings in the DATE column into actual dates.

# Let's look at them again...

retail$DATE

# What we have here are monthly figures - for each month, we just have data for the
# first day. Note that these are in the unusual American format - month / day / year. 
# This format can cause a lot of problems if you're not looking out for it.

# Lubridate gives us a set of commands: dmy(), ymd(), and mdy()
# These convert pretty much any date string into a real date, as long as it follows
# that order. So in this instance, we'll be using mdy().
# We want to take one further step, and then extract the year and month from this 
# date, ignoring the day - this will let us work with the data as purely monthly
# data.

retail <- retail %>%
  mutate(DATE = mdy(DATE)) %>%
  mutate(DATE = yearmonth(DATE))
glimpse(retail)


# Note that the DATE column type is now correctly registered as <mth>, meaning
# that it is monthly data.
# Now we can turn this into a time series as follows:

retail.ts <- retail %>%
  as_tsibble(index = DATE)

retail.ts

# Note that the first line of the output contains [1M], telling us that the 
# frequency of this data is once per month.

# The autoplot() function extends the ggplot2 functionality we've been using up
# until now, adding some nice default charts for various kinds of data. The one
# it creates for time series data is fairly easy to read.

autoplot(retail.ts)


# DECOMPOSING TIME SERIES
#########################

# Looking at this chart, it's pretty obvious that there are some patterns in this
# time series. 

# Firstly, there's a TREND: the series is rising pretty steadily, apart from an
# interruption in around 2008 (which is because of the global financial crisis).

# Secondly, there is SEASONALITY: you can see that there's quite a clear pattern
# on an annual basis, represented by regular spikes and troughs in the data.


# One useful thing to be able to do is to extract that seasonality and view it
# clearly. Here's how we would do that:

gg_season(retail.ts)

# This has extracted each year's data and put it on the graph as a separate line. You
# can see that even if the overall sales are different each year, the shape of the
# graph is very much the same - with a sharp peak in December followed by a 
# deep trough the following January.


# Given this, it's possible to entirely remove the effects of seasonality by taking the
# mean of each month in the data (or each day of the week, hour of the day, etc. - it
# depends on the frequency of your data) and subtracting it from the observed values.


# AUTOCORRELATION
#################

# With time series data, it is common to find 'autocorrelation' - meaning that the 
# variable is correlated with itself, because each subsequent observation depends on
# the previous observation (such as in a situation with a rising or a falling trend).

# This creates problems with many kinds of analysis, and removing autocorrelation from
# the data is generally preferred when working with time series data in regression
# models. This is also sometimes called "detrending" the data - removing the overall
# trend so we can observe the period-by-period variations more clearly.

# To see what degree of autocorrelation exists in our data, we can use the ACF()
# function. This is best used to plot a graph, called an ACF Plot.

retail.ts %>%
  ACF(Sales) %>%
  autoplot()

# This shows that there are high, consistent values for autocorrelation; this data
# is very heavily autocorrelated.

# We fix this by taking the first difference of the data - meaning that instead of
# absolute values, we replace each month's data with the difference (positive or
# negative) from the previous month. This preserves the detail of the data, but
# erases the overall trend - i.e., detrending.

retail.ts <- retail.ts %>%
  mutate(Sales.diff = difference(Sales))

# We can chart this...

autoplot(retail.ts, vars(Sales.diff))

# You can see that the seasonality is still there, but the overall trend has
# been removed. We can confirm this with the ACF plot:

retail.ts %>%
  ACF(Sales.diff) %>%
  autoplot()

# The ACF plot isn't completely flat - there is still noise, of course - but
# it's no longer consistent, bouncing randomly above and below zero. This means
# that there is no longer autocorrelation in the data.


# SEASONAL AND TREND DECOMPOSITION
##################################

# STL (Seasonal and Trend decomposition with LOESS) is a handy method that 
# combines various different approaches to "decomposing" a time series - splitting
# it into its trend, season, and remainder components.

retail.ts %>%
  model(stl = STL(Sales)) %>%
  components()

# Combining these commands gives us a new version of our time series, with each of
# the component parts of the data stripped out. We can graph this:

retail.ts %>%
  model(stl = STL(Sales)) %>%
  components() %>%
  autoplot()

# Here you can see that the smoothed trend and the annual cycle have been removed,
# leaving a "remainder" which is the data fluctuation that is not explained by
# these aspects. This is the part we would probably be most interested in studying
# in any further analysis of the data.




