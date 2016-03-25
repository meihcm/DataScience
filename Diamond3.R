# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
install.packages("ggplot2")
library(ggplot2)
data(diamonds)

ggplot(data=diamonds, aes(x=price)) +
  geom_histogram(aes(color=cut, stat="summary", fun.y="count"), bins = 50) +
  scale_x_continuous(breaks=c(1000,10000)) +
  facet_wrap(~ color)

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
ggplot(data=diamonds, aes(x = table, y = price,color = cut)) +
  geom_jitter(size = 3) +
  scale_x_discrete(limits = c(50,80), breaks = seq(50,80,2)) +
  scale_color_brewer(type = 'qual')

# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(data=subset(diamonds, diamonds$volume < quantile(diamonds$volume,.9)), aes(x=volume, y=price,color=clarity)) +
  scale_y_log10() +
  geom_point() +
  scale_color_brewer(type = 'div')

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

# This programming assignment WILL BE automatically graded.

# DO NOT DELETE THIS NEXT LINE OF CODE
# ========================================================================
pf <- read.delim('/datasets/ud651/pseudo_facebook.tsv')


# ENTER YOUR CODE BELOW THIS LINE
# ========================================================================

  