#-----------------------------------------------------------------------------#
# Waseda University :: School of Political Science & Economics
# - Quantitative Analysis [2024 Autumn Semester]
# Instructor: Rob Fahey <robfahey@aoni.waseda.jp>
#-----------------------------------------------------------------------------#


# WEEK 9: Advanced Regression Topics (1)


# As ever, set up your R environment and your working directory; make sure you
# edit the directory below to whereever you put the files for this class.

rm(list=ls())
library(tidyverse)
library(stargazer)

#-----------------------------------------------------------------------------#
# 1. Polynomial Regression

# Back when we looked at correlation for the first time (Week 6), we used some data
# from a package called gapminder, which gave us GDP, population, and life expectancy
# data for a large number of countries in various years.

library(gapminder)
head(gapminder)

# Let's recreate what we did in Week 6, extracting the data for 2007 and checking to
# see how GDP Per Capita is related to life expectancy.

gdp_life <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, gdpPercap)

ggplot(data = gdp_life, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  ggtitle("Scatterplot")

# Back in Week 6, we looked at the correlation between these two variables using
# Pearson's R and Spearman's rho, and found that there was a relatively strong
# and statistically significant correlation. However, it was obvious from looking
# at the scatter plot that this relationship wasn't best modelled with a straight
# line (which meant Spearman's rho was a better test of correlation, as it is a 
# non-parametric test with no assumption of linearity in the relationship).

# Now we know how to perform a regression test; how does OLS regression handle
# this data?

fit <- lm(lifeExp ~ gdpPercap, data = gdp_life)
stargazer(fit, type = "text")

# The results of the simple regression look pretty good on paper; decent R2
# values (gdpPercap explains about 45% of the variation in lifeExp), and low
# p-values for both the gdpPercap coefficient and F statistic.

# However, it's a little hard to interpret the coefficients because GDP is
# measured in dollars (so the numbers go into the tens of thousands) while
# life expectancy is measured in years (so the numbers top out at around 85).
# This difference in ranges makes things tricky because the change to life
# expectancy from a single dollar change in GDP per capita is incredibly
# tiny! Before we continue, let's fix this problem.

gdp_life <- gdp_life %>%
  mutate(gdpPercap_t = gdpPercap / 1000)

linear_model <- lm(lifeExp ~ gdpPercap_t, data = gdp_life)
stargazer(linear_model, type = "text")

# All we've done is change the GDP figure to be expressed in thousands of
# dollars. This doesn't change the model at all - note that the F Statistic
# and other measurements are exactly the same. It just shifts the scale 
# of the coefficients to be easier to read; now we can see that on average,
# a $1000 rise in GDP per capita correlates with a 0.637 year increase in
# life expectancy.

# Don't be afraid to change the scales of your own data in the same way;
# it's always easier to understand analysis results when the data is
# on relatively similar scales, and dividing a variable by a constant (in
# this case, 1000) doesn't change the results of regression analysis in
# any way.

# Let's look at this data visually.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Linear Model")


# Visual inspection reveals a problem that wasn't immediately apparent from
# the regression results - the line isn't a good match for the shape of the
# data. If the data points were just very scattered, you would get large
# residuals, meaning low R2 and high p-values. In this case, though, the
# data isn't scattered - it's just a different shape, which is actually
# more problematic, because we get a pretty good set of regression results,
# but they hide the fact that there's a systematic, non-random error in 
# our predictions. The line we've drawn tends to overestimate life expectancy
# at very low GDP levels; then underestimates it for GDPs between ~$5000 and
# $20,000; then finally starts increasingly overestimating expectancy again
# from around $40,000.

# These kinds of errors - which have a pattern, rather than being random - are
# the type we should be aiming to eliminate from our model. 

# We can see the problem very clearly by creating a graph showing all the
# residuals in our model. Remember that what we really want to see with
# residuals is "spherical errors" - normally distributed residuals
# that are totally independent of the fitted values from our model.

# We can access the fitted values of a regression model with the fitted()
# command, and the residuals with resid(). Let's make a quick data frame
# holding the output of our model, so we can easily plot the residuals.

linear_model.output = data.frame(fitted_vals = fitted(linear_model),
                                 residuals = resid(linear_model))

ggplot(data = linear_model.output, mapping = aes(x = fitted_vals, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ggtitle("Linear Model: Residuals")


# These residuals are clearly not randomly distributed. Our model has 
# significant bias, and is consistently giving positive errors in some ranges,
# and negative errors in others, which is a major problem. This could mean that
# we're missing an important variable (omitted variable bias); but from our
# visual inspection of the data, we know the first problem to fix is that
# the relationship is not linear.


# QUADRATIC REGRESSION
############################

# To address this, we can model a curve rather than a straight line by making
# adding a quadratic term to the regression model. This simply means that the
# model will now include both the predictor variable and the square of the
# predictor, which are called the "linear component" and the "quadratic 
# component" respectively.

# To add a quadratic component, we can use the I() function inside the
# model definition. This lets us do maths functions on the variables within
# the model definition - in this case, using ^2 to square the value.



quad_model <- lm(lifeExp ~ gdpPercap_t + I(gdpPercap_t^2), data = gdp_life)
stargazer(linear_model, quad_model, type = "text")

# This has significantly improved our model! The F Statistic remains 
# highly statistically significant, and the Adjusted R2 of the model
# has raised significantly - from 0.457 to 0.568. Moreover, the residual
# standard error has fallen, which means that our residuals are on 
# average smaller in the quadratic model.

# Of course, we can also plot this on a graph, by telling the geom_smooth()
# command in ggplot2 which formula we want to use. Note that when you
# are defining a formula inside a ggplot2 command sequence, you should
# use the variable names you mapped at the start of the command (x and y),
# not the variable names from the actual data frame.

# Let's also plot the original linear model line in blue for comparison.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2), 
              color = "red") +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              color = "blue") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Quadratic Model")


# The quadratic model is definitely better - the line captures the steep
# curve at the low end of the graph more accurately, as well as the plateau
# in the numbers at higher income levels. It's still not ideal, though,
# as this kind of curve ends up predicting a downturn at the upper end
# of the range - so we would be predicting that in very high income
# countries, life expectancy will start to fall again as income continues
# to rise, which seems relatively unlikely (and clearly isn't reflected
# in the actual data).


# POLYNOMIAL REGRESSION
############################

# This data might be better represented by a more complex kind of curved
# line - one defined by a polynomial regression, which includes powers 
# of x beyond the square (power of 2) function in a quadratic term.

# R has a helpful function called poly() which will automatically add all
# of the required power terms to the model definition, so for example:
#      y ~ poly(x, 3)
# is functionally the same as writing out:
#     y ~ x + I(x^2) + I(x^3)

# Let's try it, just repeating our quadratic model using this new function.


poly_model <- lm(lifeExp ~ poly(gdpPercap_t, 2), data = gdp_life)
stargazer(linear_model, quad_model, poly_model, type = "text")

# Something is odd here - the coefficients we get from the polynomial
# model are very different from the ones we got from the quadratic
# model. Aren't these meant to be the same?

# If you look at the details of the model (R2, Adjusted R2, Residual
# Standard Error, F Statistic), you see that they're identical between
# the quadratic and polynomial model. What has happened here is that the
# poly function has by default applied a mathematical transformation
# to the data to create something called orthagonal polynomials, which
# maintains all the important aspects of the model but improves it by
# making the p-values of all the different power levels directly
# interpretable. Without doing this, we can't really interpret the
# p-values and figure out which parts of the polynomial equation are
# improving our model.

# To confirm that the two models - using poly() and using I(x^2) - are
# actually identical, let's try plotting them both on the graph.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "red") +
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2), 
              color = "blue", linetype = "dashed") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("poly(x, 2) vs. I(x^2)")

# You can see that they overlap perfectly - these are the same models.
# If you want to see the poly() result without the orthographic
# transformation for some reason, you can add "raw = TRUE" to the
# function call:

poly_model.raw <- lm(lifeExp ~ poly(gdpPercap_t, 2, raw = TRUE), data = gdp_life)
stargazer(linear_model, quad_model, poly_model, poly_model.raw, type = "text")


# Okay, now that we know how to replicate our quadratic model using the poly()
# function, let's see how it works with higher level polynomials. We're first
# going to try just adding one more level to the polynomial - a power of 3,
# which changes this from a quadratic function to a cubic function.

poly_model.3 <- lm(lifeExp ~ poly(gdpPercap_t, 3), data = gdp_life)
stargazer(linear_model, poly_model, poly_model.3, type = "text")

# The results suggest that this has improved the model once more. 
# Let's plot it and have a look.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("3rd-Order Polynomial (Cubic) Model")

# This is definitely better - the plateau in the data is being captured more
# clearly. Could it be improved by adding more levels to the polynomial?


# To test this, we can try creating models with many different polynomial
# levels, and gradually reduce the number of levels until we reach the
# point where the largest polynomial term in our model is statistically
# significant.

# You could do this manually (starting from a polynomial of 10 and working
# downwards until you find a model where the highest polynomial is significant),
# or you could do it in a loop in R:

poly_results <- tibble(
  Level = numeric(),
  AdjR2 = numeric(),
  Significance = numeric(),
  Stars = character()
)
for (polycount in 10:3) {
  fit <- lm(lifeExp ~ poly(gdpPercap_t, polycount), data = gdp_life)
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

# To clarify what's happening here - we are creating an empty table with the
# tibble() command and specifying the columns that will hold our results. Next,
# we're iterating through every number from 10 backwards to 3, and creating
# a polynomial model (with lm()) for that number. We pull the p-value for
# the last coefficient (which will be the highest polynomial) and the adjusted
# R2 for each of these models, and put it into the blank table. Then we
# can view a quick summary of our results, and see which models provide the
# best fit.

# According to this table, it looks like the right number of polynomials to
# use is 5. No level above that is significant. 

# Let's take a look at all those different models.

poly_model.2 <- lm(lifeExp ~ poly(gdpPercap_t, 2), data = gdp_life)
poly_model.3 <- lm(lifeExp ~ poly(gdpPercap_t, 3), data = gdp_life)
poly_model.4 <- lm(lifeExp ~ poly(gdpPercap_t, 4), data = gdp_life)
poly_model.5 <- lm(lifeExp ~ poly(gdpPercap_t, 5), data = gdp_life)
stargazer(poly_model.2, poly_model.3, poly_model.4, poly_model.5,
          type = "text")

# In each of these models, all of the levels of the polynomial are significant;
# even if one of them was not significant, we should still generally use the 
# polynomial with the highest significant level (5 in this case).

# Let's see how that line actually looks on the graph.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 5), 
              color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("5th-Order Polynomial")


# This is quite a good match for the data - probably as good as we're going to
# get with a polynomial model. Our Adjusted R2 is now 0.651, so we are explaining
# almost two thirds of the variation in life expectancy based on a function of
# the GDP per capita. However, this might be a little tricky to interpret and
# explain in our research; the plateaus in the data make some sense, but features
# like the sharp dip between $30,000 and $45,000 make less sense, so it could
# be hard to justify choosing a regression line with this feature in it.


# Before we move on, let's quickly take a look at the residuals from the
# new polynomial regression line.

poly_model.5.output = data.frame(fitted_vals = fitted(poly_model.5),
                                 residuals = resid(poly_model.5))

ggplot(data = poly_model.5.output, mapping = aes(x = fitted_vals, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ggtitle("5th-Order Polynomial Model: Residuals")


# These errors are still not spherical - but this is much closer to being a normal
# distribution of the residuals. Most errors are around the 0 line, with only a few
# outliers. This is a much better model of the relationship between these variables
# than the one we started with.



# LOGARITHMIC TRANSFORMATION
############################

# Polynomials are not the only form of curve function that can be added to a regression
# model. We can also use the natural logarithm of a variable to model a relationship
# where the effect is very strongly positive (or negative) at low values, but gradually
# becomes less significant as the values increase.

# This is a very effective transformation to apply when your data is highly skewed; in
# those cases, the natural logarithm of the data might be much more like a normal
# distribution than the original data was (this is actually a distribution called 
# log-normal, meaning that its logarithm is normal).

# For example, look at how our GDP / life expectancy data changes when we use a
# logarithmic transformation...

ggplot(data = gdp_life, mapping = aes(x = log(gdpPercap_t), y = lifeExp)) +
  geom_point() +
  ggtitle("GDP Per Capita data, unskewed with natural logarithm")

# The heavy skew to the left-hand side of the data is gone. The data is much closer to
# being normally distributed with a log trnsformation, so let's try modelling a 
# logarithmic regression and see how it compares to our polynomial. Specifically, the
# regression we're doing here is a linear-log regression, because our outcome variable
# is still linear - we haven't transformed it.

linlog_model <- lm(lifeExp ~ log(gdpPercap_t), data = gdp_life)
stargazer(linear_model, poly_model.2, poly_model.5, linlog_model,
          type = "text", 
          column.labels = c("Linear", "Quadratic", "5th Polynomial", "Linear-Log"))

# Despite only having one simple term, the logarithmic model actually has a higher 
# adjusted R2, and lower residual standard errors, than the complex 5-level polynomial
# model. Of course, it is also much better than the simple linear model or the
# quadratic model.

# Let's look at them on a graph for comparison.

ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ log(x), 
              color = "red") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 5), 
              color = "blue") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Logarithm vs. 5th-Order Polynomial")


# The shape of the linear-log curve is a good fit for this data, but of
# course there would be many cases where it doesn't fit well, as it only has
# one curve, compared to the more complex polynomial shape.


# The logarithmic transformation of the predictor variable is only one 
# potential use of logarithms in regression. It's also possible to 
# fit an exponential model, which involves a log transformation of the
# outcome variable y. This is good for situations where growth begins
# slowly and then accelerates. That doesn't really describe our data
# (in our case, growth begins fast and then slows down), so unsurprisingly
# an exponential model is a bad fit here.

expon_model <- lm(log(lifeExp) ~ gdpPercap_t, data = gdp_life)
stargazer(poly_model.5, linlog_model, expon_model,
          type = "text", 
          column.labels = c("5th Polynomial", "Linear-Log", "Exponential"))


ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_line(data = data.frame(x = expon_model$model$gdpPercap_t,
                              y = exp(predict(expon_model))),
            mapping = aes(x = x, y = y),
            color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Log-Linear Model")


# We can also use logarithms on both sides of the equation, which can help
# in situations where both variables are strongly skewed - i.e., the data
# falls heavily on one side of the scale, rather than being normally
# distributed in the middle.

# This is broadly true of our data, so the log-log model gives a good result,
# but it's not an improvement over the basic logarithmic (lin-log) model.


loglog_model <- lm(log(lifeExp) ~ log(gdpPercap_t), data = gdp_life)
stargazer(poly_model.5, linlog_model, expon_model, loglog_model,
          type = "text",
          column.labels = c("5th Polynomial", "Linear-Log", "Exponential", "Log-Log"))


ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_line(data = data.frame(x = exp(loglog_model$model$`log(gdpPercap_t)`),
                              y = exp(predict(loglog_model))),
            mapping = aes(x = x, y = y),
            color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Log-Log Model")


# RECIPROCAL TRANSFORMATION
###########################

# Finally, one more transformation that can be performed to a variable when
# the relationship with the outcome is curved is the reciprocal transformation.
# This basically means calculating 1/x, and it creates a curve where there is
# a very sharp rise (or fall) in the initial part of the graph, followed by
# hitting a ceiling level / plateau.

# That also sounds like a reasonable description of our data (which makes sense,
# if we assume that GDP can only lift life expectancy up to a certain level beyond
# which just getting richer doesn't make a difference - everyone at this level, no 
# matter how wealthy their nation is, is relying on medical advancements to create 
# further life expectancy increases). Let's model it and see how it looks.

reciprocal_model <- lm(lifeExp ~ I(1/gdpPercap_t), data = gdp_life)
stargazer(linear_model, poly_model.5, linlog_model, reciprocal_model,
          type = "text", 
          column.labels = c("Linear", "Polynomial-5", "Lin-Log", "Reciprocal"))

# The reciprocal model actually turns out to be a bad fit here. We can see why
# when we plot the graph; although some of the assumptions it makes are reasonable,
# the equation isn't a good fit for the actual shape of this curve, which has a
# less extreme bend in the data than reciprocal transformation creates.


ggplot(data = gdp_life, mapping = aes(x = gdpPercap_t, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ I(1/x), 
              color = "red") +
  xlab("GDP Per Capita ($1000 units)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Reciprocal Transformation Model")
