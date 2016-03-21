# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

# Go to the discussions to
# share your thoughts and to discover
# what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.

# SUBMIT YOUR CODE BELOW THIS LINE
# ===================================================================
install.packages("ggplot2")
library(diamonds)
## 1. Price/Carat diamond by colors ##
## validation ##
by(diamonds$price/diamonds$carat, diamonds$color, summary)
## Zoom to 1st quartile and 4th quartile ##
qplot(data=diamonds, x=color, y=price/carat, xlab="Diamond Color", ylab="Price Per Carat ($)") + 
  geom_boxplot() +
  coord_cartesian(ylim=c(700,5500))
ggsave('Diamond1_priceHistory.png')

## 2. Frequency polygon of carats ##
## Bound to count >= 2000 ##
ggplot(diamonds, aes(carat)) +
  geom_freqpoly(binwidth=.01) +  
  coord_cartesian(ylim=c(2000,3000), xlim=c(0,2))
ggsave('Diamond1_caratGreater2000.png')
