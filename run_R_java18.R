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