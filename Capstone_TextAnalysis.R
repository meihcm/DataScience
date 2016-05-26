## START SETUP ENVIRONMENT ##
## To run R in java 1.8 ##
## 1. run from command line
## LD_LIBRARY_PATH=$(/usr/libexec/java_home)/jre/lib/server: open -a RStudio'
## Also run this in command line: sudo R CMD javareconf ##
## 2. Run in R studio 
install.packages('rJava', type='source')

## 3. Proving that it works with stanford's nlp
devtools::install_github("statsmaths/coreNLP", force=TRUE)
download.file("http://nlp.stanford.edu/software/stanford-corenlp-full-2015-01-29.zip",destfile="stanford-corenlp-full-2015-01-29.zip") 
unzip("stanford-corenlp-full-2015-01-29.zip") 
library(coreNLP) 
initCoreNLP("stanford-corenlp-full-2015-01-29") 
## END SETUP ENVIRONMENT ##

library("data.table")
library("dplyr")
install.packages("gdata")
library("gdata")
library("XML")
library(stringr)
library(curl)
library(httr)

projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)

## FUNCTIONS ##
## This function uses stanford's nlp and sentiment analysis to get a mean sentiment score ##
getSentimentScore <- function(textStr) {
  sentimentScore = 0 # neutral
  if(!is.na(textStr) && !is.null(textStr) && trimws(textStr) != "") {
    output = annotateString(textStr)
    sentiment <- getSentiment(output)   
    sentimentScore = mean(sentiment$sentimentValue)
  }
  
  return(sentimentScore)
}

## MAIN ##
## We will be running in batches of 100, each batch will both do url craw of the content
## And 
row_batches=100

## 400 X row_batches = 40,000
for (outerCounter in 1:400) 
{
  ## Print output for long runs so that we know it is still working ##
  print(paste("Start Time:",Sys.time()))
  
  startStep = (outerCounter * row_batches) + 1 
  maxStep = startStep + row_batches  - 1
  ## chunk the process ##
  ## See what is the new starting index to read from the dataframe for url crawling
  if(outerCounter > 0)
  {
    skipRow = startStep
  }  else {
    skipRow = startStep - 1
  }
  if(skipRow == 0) {
    print(paste("Reading from csv file skipping (1st time): ", skipRow, ", batch of:" , row_batches))
    mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", skip = skipRow, nrows=row_batches, header = TRUE)
    ## Save header for subsequent batch run
    header = names(mashable_df)
    ## Add additional colums for sentiment analysis
    namevector = c("title", "title_sentiment","para1","para1_sentiment", "para2","para2_sentiment", "para3","para3_sentiment", "full_sentiment")
    mashable_df[,namevector] <- NA
  } else {
    print(paste("Reading from csv file skipping: ", skipRow, ", batch of:" , row_batches))
    mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", skip = skipRow, nrows=row_batches, header = FALSE)
    ## Rebuild header column names for subsequent runs
    names(mashable_df) <- header
    mashable_df[,namevector] <- NA
  }
  ## Inner loops look at each 100 batches and does both url mining,
  ## Sentiment analysis, and append to an offline file to maximize
  ## efficient use of memory
  for(counter in 1:row_batches) 
  {
    thisUrl = mashable_df$url[counter]
    ## Run url mining for this set of urls
    response <- GET(as.character(thisUrl))
    ## Make sure url get returns success code
    if (response$status_code != 200) {
      print(paste("Detected error url(skipping:",thisUrl))
      next
    }
    ## Parse the html
    doc.html = htmlTreeParse(thisUrl,useInternal = TRUE,encoding = "UTF-8")
    # Extract all the paragraphs (HTML tag is p, starting at
    # the root of the document). Unlist flattens the list to
    # create a character vector.
    ## This is title of article as marked-up by h1 tags
    doc.title = unlist(xpathApply(doc.html, '//h1', xmlValue))
    ## This is paragraph(s) of article as marked-up by p tags
    doc.text = unlist(xpathApply(doc.html, '//section/p', xmlValue))
    title = doc.title[2]
    title = str_replace_all(title, "[\r\n]" , " ")
    nextParagraphIndex = 1
    ## Try to find 1st paragraph
    for(i in nextParagraphIndex:length(doc.text))
    {
      paragraph1 = doc.text[i]
      if(!is.null(paragraph1) && !is.na(paragraph1) && trimws(paragraph1) != "") {
        paragraph1 = str_replace_all(paragraph1, "[\r\n]" , " ")
        paragraph1 = str_replace_all(paragraph1, "[\"]" , "<dquote>")
        paragraph1 = str_replace_all(paragraph1, "[']" , "<squote>")        
        nextParagraphIndex = i + 1
        break ##break out of this loop because we have a paragraph
      } else {
        paragraph1 = ""
      }
    }
    ## Try to find 2nd paragraph
    for(i in nextParagraphIndex:length(doc.text))
    {
      paragraph2 = doc.text[i]
      if(!is.null(paragraph2) && !is.na(paragraph2) && trimws(paragraph2) != "") {
        paragraph2 = str_replace_all(paragraph2, "[\r\n]" , " ")
        paragraph2 = str_replace_all(paragraph2, "[\"]" , "<dquote>")
        paragraph2 = str_replace_all(paragraph2, "[']" , "<squote>")             
        nextParagraphIndex = i + 1
        break ##break out of this loop because we have a paragraph
      } else {
        paragraph2 = ""
      }
    }
    ## Sometimes 2nd paragraph as represented by <p> is just credit, so ignore if so
    if(length(a <- grep('courtesy of', paragraph2,ignore.case = TRUE)))
       paragraph2 = ""
    ## Try to find 3rd paragraph
    for(i in nextParagraphIndex:length(doc.text))
    {
      paragraph3 = doc.text[i]
      if(!is.null(paragraph3) && !is.na(paragraph3) && trimws(paragraph3) != "") {
        paragraph3 = str_replace_all(paragraph3, "[\r\n]" , " ")
        paragraph3 = str_replace_all(paragraph3, "[\"]" , "<dquote>")
        paragraph3 = str_replace_all(paragraph3, "[']" , "<squote>")
        nextParagraphIndex = i + 1
        break ##break out of this loop because we have a paragraph
      } else {
        paragraph3 = ""
      }
    }
    ## Sometimes 3rd paragraph as represented by <p> is just credit, so ignore if so
    if(length(a <- grep('courtesy of', paragraph3,ignore.case = TRUE)))
      paragraph3 = ""
    
    ## do Sentiment ##
    mashable_df$title_sentiment[counter] = getSentimentScore(title)
    mashable_df$para1_sentiment[counter] = getSentimentScore(paragraph1)
    mashable_df$para2_sentiment[counter] = getSentimentScore(paragraph2)
    mashable_df$para3_sentiment[counter] = getSentimentScore(paragraph3)
    mashable_df$full_sentiment[counter] = getSentimentScore(doc.text)
    
    ## every 20 print out something, make sure processing is still alive
    if(counter %% 20 == 0) {
      print(paste("Finished up to: ", counter))
    }
  }
  ## Now do some special escapes to make sure we can read it back once written to file
  title = str_replace_all(title, "[\"]" , "<dquote>")
  title = str_replace_all(title, "[']" , "<squote>")        
  if(!is.null(paragraph1) && !is.na(paragraph1) && trimws(paragraph1) != "") {
    paragraph1 = str_replace_all(paragraph1, "[\"]" , "<dquote>")
    paragraph1 = str_replace_all(paragraph1, "[']" , "<squote>")        
  }
  if(!is.null(paragraph2) && !is.na(paragraph2) && trimws(paragraph2) != "") {
    paragraph2 = str_replace_all(paragraph2, "[\"]" , "<dquote>")
    paragraph2 = str_replace_all(paragraph2, "[']" , "<squote>")        
  }
  if(!is.null(paragraph3) && !is.na(paragraph3) && trimws(paragraph3) != "") {
    paragraph3 = str_replace_all(paragraph3, "[\"]" , "<dquote>")
    paragraph3 = str_replace_all(paragraph3, "[']" , "<squote>")        
  }
  mashable_df$title[counter] = title
  mashable_df$para1[counter] = paragraph1
  mashable_df$para2[counter] = paragraph2
  mashable_df$para3[counter] = paragraph3
  
  print("about to write to file")
  ## Chunk the save by appending ##
  if(outerCounter > 0) 
  {
    write.table(mashable_df, sep='^', append=TRUE, file="mashable_engineered.tbl", row.names=FALSE, col.names=FALSE, quote = TRUE)
  } else {
    write.table(mashable_df, sep='^', file="mashable_engineered.tbl", row.names=FALSE,quote=TRUE)
  }
  print(paste("End Time:",Sys.time()))
} ## outer for
