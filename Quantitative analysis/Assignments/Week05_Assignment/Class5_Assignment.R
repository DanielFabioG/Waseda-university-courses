# Class 5 Assignment

# The file reported_income.csv contains a sample of 100 observations,
# taken at random from a much larger population, of the annual income
# of people who have completed an undergraduate degree ("UniGrad"), 
# and those who have only graduated high school ("HSGrad"). There are
# two columns, one for education level and one for income.

# Load this file into R, and then write code to perform the following
# analyses.

# Loading tidyverse and the data from csv file
library(tidyverse)

# To make the data reproducible I will download it from github instead of reading it from a local file
url <- "https://raw.githubusercontent.com/DanielFabioG/Waseda-university-courses/refs/heads/main/Quantitative%20analysis/Assignments/Week05_Assignment/reported_income.csv"
income_df <- read_csv(url)

# Removing scientific notation, since I am a human and I like to read numbers that my brain can understand
# honestly this is just for the low p-value to be more readable
options(scipen = 999)

# Renaming the data in rows
income_df <- income_df %>%
  mutate(education = ifelse(education == "UniGrad", "University graduate", "High school graduate"))


# 1) Create a boxplot and a histogram which display this data. You
#    should be able to see both groups (university graduates and high
#    school graduates) clearly in a single chart.
#    Note that there are far more observations in this data than in
#    the data we used in class, so you may need to change settings
#    like the number of "bins" in the histogram to make it look good.

# Creating the boxplot using ggplot

income_df %>%
  ggplot(aes(x = education, y = income)) +
  #adding some color to the boxplot
  geom_boxplot(aes(fill = education)) +
  #adding some jitter to the boxplot, this can be visualised better this way since the data is discrete
  geom_jitter(aes(color = education), width = 0.2) +
  # changing the title and labels
  labs(title = "Income distribution by education level",
       x = "Education level",
       y = "Income",
       caption = "Based on data from professor Robert")+
  theme_minimal()+
  # this is to remove the legend since its not necessary here
  theme(legend.position = "none")+
  # adding EUR and commas to the y axis to make it more readable
  scale_y_continuous(labels = scales::label_currency(prefix = "€"))+
  # adding colors
  scale_fill_manual(values = c("palevioletred", "cornflowerblue"))+
  scale_color_manual(values = c("cornflowerblue", "palevioletred"))


# Creating the histogram using ggplot
# most of the same as above but just with a histogram
income_df %>%
  ggplot(aes(x = income, fill = education)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "stack") +
  labs(title = "Income distribution by education level",
       x = "Income",
       y = "Frequency",
       caption = "Based on data from professor Robert")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("palevioletred", "cornflowerblue"))



# 2) Find out, using a t-test, if there is a statistically significant 
#    difference between the two groups. 

# I will use the t.test function to compare the two groups
t.test(income ~ education, data = income_df)

# The p-value is less than 0.00005, so we can mega reject the null hypothesis that the two groups have the same mean income
# This means that there is a statistically significant difference between income by education level

# 3) The National Statistics Office has released data showing that the
#    national mean income is €33,000.
#    - Does this data support that claim? Use a statistical test to find
#      out if your data fits the NSO's information.
#    - Can you think of some explanation for your finding?
#      Write your thoughts / explanation in a short comment below.


# I will use the t.test function to compare the two groups to the national mean
t.test(income_df$income, mu = 33000)

# The p-value is less than 0.05, so we can reject the null hypothesis that this sample size of data has the same mean income as the national mean income

# I will now test by looking at only the high school graduates
t.test(income_df$income[income_df$education == "High school graduate"], mu = 33000)

# Little bit better but still not enough to reject the null hypothesis

# The reason for this is that the sample size is too small 
# and the national mean income is representative of the entire population
# while this sample is just a small subset of the population which have finished at least high school
# while the national mean income is for people with no restrictions on education level so they can also have 0 education level etc

