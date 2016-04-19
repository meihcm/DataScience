library("data.table")
library("dplyr")
library("gdata")
install.packages("XML")
library("XML")

projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)
mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", header = TRUE)
namevector = c("title", "title_sentiment","para1","para1_sentiment", "para2","para2_sentiment", "para3","para3_sentiment")
mashable_df[,namevector] <- NA
counter = 1
for(thisUrl in mashable_df$url) 
{
  doc.html = htmlTreeParse(thisUrl,useInternal = TRUE)
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  doc.title = unlist(xpathApply(doc.html, '//h1', xmlValue))
  doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
  title = doc.title
  paragraph1 = doc.text[1]
  paragraph2 = doc.text[2]
  paragraph3 = doc.text[3]
  mashable_df$title[counter] = title
  mashable_df$para1[counter] = paragraph1
  mashable_df$para2[counter] = paragraph2
  mashable_df$para3[counter] = paragraph3
  
  ##counter = counter + 1
  ##if (counter > 1)
  ##  break
}

