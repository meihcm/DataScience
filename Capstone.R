library("data.table")
library("dplyr")
library("gdata")
install.packages("XML")
library("XML")

projectHome <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
datasetHome <- paste(projectHome,"/OnlineNewsPopularity",sep="")
setwd(datasetHome)
row_batches=100

for (outerCounter in 0:400) 
{
  startStep = (outerCounter * row_batches) + 1 
  maxStep = startStep + row_batches  - 1
  ## chunk the process ##
  skipRow = startStep - 1
  if(skipRow == 0) {
    print(paste("Reading from csv file skipping (1st time): ", skipRow, ", batch of:" , row_batches))
    mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", skip = skipRow, nrows=row_batches, header = TRUE)
    header = names(mashable_df)
    namevector = c("title", "title_sentiment","para1","para1_sentiment", "para2","para2_sentiment", "para3","para3_sentiment")
    mashable_df[,namevector] <- NA
  } else {
    print(paste("Reading from csv file skipping: ", skipRow, ", batch of:" , row_batches))
    mashable_df = read.csv("OnlineNewsPopularity.csv", sep=",", skip = skipRow, nrows=row_batches, header = FALSE)
    names(mashable_df) <- header    
  }
  
  for(counter in 1:row_batches) 
  {
    thisUrl = mashable_df$url[counter]
    print(paste("about to read from url:",thisUrl))
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
      if(!is.null(paragraph1) && !is.na(paragraph1) && trimws(paragraph1) != "") {
        nextParagraphIndex = i + 1
        break ##break out of this loop because we have a paragraph
      }
    }
    for(i in nextParagraphIndex:length(doc.text))
    {
      paragraph2 = doc.text[i]
      if(!is.null(paragraph2) && !is.na(paragraph2) && trimws(paragraph2) != "") {
        nextParagraphIndex = i + 1
        break ##break out of this loop because we have a paragraph
      }
    }
    if(length(a <- grep('courtesy of', paragraph2,ignore.case = TRUE)))
       paragraph2 = ""
    for(i in nextParagraphIndex:length(doc.text))
    {
      paragraph3 = doc.text[i]
      if(!is.null(paragraph3) && !is.na(paragraph3) && trimws(paragraph3) != "") {
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
  print("about to write to file")
  ## Chunk the save by appending ##
  if(outerCounter > 0) 
  {
    write.table(mashable_df, sep='^', append=TRUE, file="mashable_engineered.tbl", row.names=FALSE, col.names=FALSE)
  } else {
    write.table(mashable_df, sep='^', file="mashable_engineered.tbl", row.names=FALSE)
  }
} ## outer for
