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
maxStep = 100
for(counter in 1:maxStep) 
{
  thisUrl = mashable_df$url[counter]
  doc.html = htmlTreeParse(thisUrl,useInternal = TRUE)
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  doc.title = unlist(xpathApply(doc.html, '//h1', xmlValue))
  doc.text = unlist(xpathApply(doc.html, '//section/p', xmlValue))
  title = doc.title[2]
  nextParagraphIndex = 1
  for(i in nextParagraphIndex:length(doc.text))
  {
    paragraph1 = doc.text[i]
    if(trimws(paragraph1) != "") {
      nextParagraphIndex = i + 1
      break ##break out of this loop because we have a paragraph
    }
  }
  for(i in nextParagraphIndex:length(doc.text))
  {
    paragraph2 = doc.text[i]
    if(trimws(paragraph2) != "") {
      nextParagraphIndex = i + 1
      break ##break out of this loop because we have a paragraph
    }
  }
  if(length(a <- grep('courtesy of', paragraph2,ignore.case = TRUE)))
     paragraph2 = ""
  for(i in nextParagraphIndex:length(doc.text))
  {
    paragraph3 = doc.text[i]
    if(trimws(paragraph3) != "") {
      nextParagraphIndex = i + 1
      break ##break out of this loop because we have a paragraph
    }
  }
  if(length(a <- grep('courtesy of', paragraph3,ignore.case = TRUE)))
    paragraph3 = ""
  
  mashable_df$title[counter] = title
  mashable_df$para1[counter] = paragraph1
  mashable_df$para2[counter] = paragraph2
  mashable_df$para3[counter] = paragraph3
}

