# Class 5 Assignment

# The file reported_income.csv contains a sample of 100 observations,
# taken at random from a much larger population, of the annual income
# of people who have completed an undergraduate degree ("UniGrad"), 
# and those who have only graduated high school ("HSGrad"). There are
# two columns, one for education level and one for income.

# Load this file into R, and then write code to perform the following
# analyses.

# Loading tidyverse and the data from csv
library(tidyverse)

# To make the data reproducible I will download it from github instead of reading it from a local file
url <- "https://raw.githubusercontent.com/DanielFabioG/Waseda-university-courses/refs/heads/main/Quantitative%20analysis/Assignments/Week05_Assignment/reported_income.csv"
income_df <- read_csv(url)


# 1) Create a boxplot and a histogram which display this data. You
#    should be able to see both groups (university graduates and high
#    school graduates) clearly in a single chart.
#    Note that there are far more observations in this data than in
#    the data we used in class, so you may need to change settings
#    like the number of "bins" in the histogram to make it look good.




# 2) Find out, using a t-test, if there is a statistically significant 
#    difference between the two groups. 




# 3) The National Statistics Office has released data showing that the
#    national mean income is €33,000.
#    - Does this data support that claim? Use a statistical test to find
#      out if your data fits the NSO's information.
#    - Can you think of some explanation for your finding?
#      Write your thoughts / explanation in a short comment below.




