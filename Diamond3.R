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
pf <- read.delim('https://s3.amazonaws.com/udacity-hosted-downloads/ud651/pseudo_facebook.tsv')


# ENTER YOUR CODE BELOW THIS LINE
# ========================================================================
pf$prop_initiated <- ifelse(pf$friend_count > 0, round(pf$friendships_initiated/pf$friend_count,2), NaN) 
  
# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================================
pf$year_joined <- (2014 - floor(pf$tenure/365))
pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))
ggplot(subset(pf, tenure > 0)) +
  geom_line(aes(x=tenure,y=prop_initiated, color=year_joined.bucket),stat='summary', fun.y=median,na.rm = TRUE)

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

# There won't be a solution image for this exercise.
# You will answer some questions about your plot in
# the next two exercises.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ====================================================
pf$year_joined <- (2014 - floor(pf$tenure/365))
pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))
ggplot(subset(pf, tenure > 0),aes(x=tenure,y=prop_initiated),stat='summary', fun.y=median) +
  #geom_line(stat='summary', fun.y=median) +
  geom_smooth(aes(color = year_joined.bucket),na.rm=TRUE)

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

# Note: In the link, a color palette of type
# 'div' was used to color the histogram using
# scale_color_brewer(type = 'div')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
diamonds$price_carat <- round(diamonds$price / diamonds$carat,2)
ggplot(diamonds, aes(x=cut, y=price_carat, color=color)) +
  geom_point(stat = 'identity') +
  facet_wrap(~ clarity) +
  scale_color_brewer(type='div') +
  geom_jitter(size=1)

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 4 or you can start fresh and choose a different
# data set from Gapminder.

# If you’re feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ============================================================================================

