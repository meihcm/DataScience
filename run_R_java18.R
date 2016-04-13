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

## Diamonds as randomForest
data=diamonds
set.seed(42)
rownames(data)<-1:nrow(data)
rows <- sample(x=1:nrow(data),size=0.7 *  nrow(data))

train <- data[rows,]
test <-data
install.packages("randomForest")
library(randomForest)

set.seed(42)
model0<-randomForest(price~.,data=train,replace=T,ntree=100)

imp<-importance(model0)
vars<-dimnames(imp)[[1]]
imp<-data.frame(vars=vars,imp=as.numeric(imp[,1]))
imp<-imp[order(imp$imp,decreasing=T),]

par(mfrow=c(1,2))
varImpPlot(model0,main='Variable Importance Plot: Base Model')
plot(model0,main='Error vs No. of trees plot: Base Model')

pred<-predict(object=model0,newdata=test)
actual<-test$price
result<-data.frame(actual=actual,predicted=pred)
model0
pred

ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.7)+
  ggtitle('Plotting Error')

