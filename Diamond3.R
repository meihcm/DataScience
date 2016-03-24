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
table_diamonds <- as.data.frame(table(diamonds$price, diamonds$cut))
names(table_diamonds) <- c("Price","Cut","Freq")
table_diamonds <- table_diamonds[table_diamonds$Freq > 0 & table_diamonds$Freq <= 80,]
ggplot(data=table_diamonds, aes(x=Freq,y=Price)) +
  geom_point(aes(colour = Cut)) +
  scale_x_continuous(limits=c(50,80), breaks=2) +
  scale_color_brewer(type = 'qual')
  