# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
ggplot(data=diamonds, aes(y=price, x=x)) +
  geom_point()

## Correlation test ## price and x
cor.test(diamonds$x,diamonds$price)

## Correlation test ## price and y
cor.test(diamonds$y,diamonds$price)

## Correlation test ## price and z
cor.test(diamonds$z,diamonds$price)

# Create a simple scatter plot of price vs depth.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
#==================================================
ggplot(data=diamonds, aes(y=price, x=depth)) +
  geom_point()

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.

# This assignment is not graded and
# will be marked as correct when you submit.

# ALTER THE CODE BELOW THIS LINE
#============================================
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=1/100) + 
  scale_x_continuous(breaks = seq(0,80,by=2))

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ========================================
ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_point() +
  scale_y_continuous(limits=c(0,quantile(diamonds$price,c(.99))))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# =================================================================
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(data=diamonds, aes(x=volume, y=price)) +
  geom_point()

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

# We encourage you to think about this next question and
# to post your thoughts in the discussion section.

# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ========================================
diamonds2 <- subset(diamonds, volume < 800 & volume > 0)
ggplot(data=diamonds2, aes(x=volume, y=price)) +
  geom_point(alpha=1/100) +
  geom_smooth()
  
# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

# This assignment WILL BE automatically
# graded!

# DO NOT ALTER THE NEXT THREE LINES OF CODE.
# ======================================================
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
diamondsByClarity <- diamonds %>% group_by(clarity) %>% summarize(mean(price), median(price),min(price), max(price), n())
names(diamondsByClarity) <- c("clarity", "mean_price", "median_price", "min_price", "max_price", "n")

# We’ve created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

# This assignment is not graded and
# will be marked as correct when you submit.

# See the Instructor Notes for more info on bar charts
# and for a hint on this task.

# DO NOT DELETE THE LINES OF CODE BELOW
# ===================================================================
install.packages("gridExtra")
data(diamonds)
library(dplyr)
library(gridExtra)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

# ENTER YOUR CODE BELOW THIS LINE
# ===================================================================
bar1 <- ggplot(data=diamonds_mp_by_clarity, aes(x=clarity,y=mean_price)) + 
  geom_bar(stat="identity")
bar2 <- ggplot(data=diamonds_mp_by_color, aes(x=color,y=mean_price)) + 
  geom_bar(stat="identity")
grid.arrange(bar1, bar2)

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you’re feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================

