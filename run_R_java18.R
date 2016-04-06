## To run R in java 1.8 ##
## 1. run from command line
## LD_LIBRARY_PATH=$(/usr/libexec/java_home)/jre/lib/server: open -a RStudio'

## 2. Run in R studio 
install.packages('rJava', type='source')
## 3. Proving that it works with stanford's nlp
devtools::install_github("statsmaths/coreNLP", force=TRUE)
download.file("http://nlp.stanford.edu/software/stanford-corenlp-full-2015-01-29.zip") 
unzip("stanford-corenlp-full-2015-01-29.zip") 
library(coreNLP) 
initCoreNLP("stanford-corenlp-full-2015-01-29") 

## Example parsing of url
doc.html = htmlTreeParse('http://mashable.com/2013/01/07/amazon-instant-video-browser/#SvEM_K3nFuqq',
                         useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.title = unlist(xpathApply(doc.html, '//h1', xmlValue))
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text = c(doc.title[2],".\n", doc.text)
# Replace all \n by spaces
# doc.text = gsub('\\n', ' ', doc.text)

# Join all the elements of the character vector into a single
# character string, separated by spaces
doc.text = paste(doc.text, collapse = ' ')
write(doc.text, file="www.nbcnewyork.com.html")

## Now run
output = annotateString(doc.text)
sentiment <- getSentiment(output)