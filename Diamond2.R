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
  