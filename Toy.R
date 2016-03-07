## SETUP
library("data.table")
library("dplyr")
library("gdata")
PROJECT_HOME <- paste("~/DataScience") ##"/Users/michaelchiem/DataScience"
DATASET_HOME <- paste(PROJECT_HOME,"/",sep="")

## 0: Load the data in RStudio
setwd(DATASET_HOME)
REFINE_FILE = read.xls("refine.xlsx", sheet = 1, header = TRUE)

## Save the data set as a CSV file called refine_original.csv and load it in RStudio into a data frame.
write.table(REFINE_FILE, file="refine_original.csv")
REFINE_ORIG_FILE = read.table(file="refine_original.csv",header=TRUE)
## 1: Clean up brand names
## Clean up the 'company' column, so all of the misspellings of the brand names are standardized. 
## For example, you can transform the values in the column to be: 
## philips, akzo, van houten and unilever (all lowercase)
REFINE_ORIG_FILE$company <- as.character(REFINE_ORIG_FILE$company)
REFINE_ORIG_FILE$company[tolower(REFINE_ORIG_FILE$company) %like% "ips"] <- "philips"
REFINE_ORIG_FILE$company[tolower(REFINE_ORIG_FILE$company) %like% "ak"] <- "akzo"
REFINE_ORIG_FILE$company[tolower(REFINE_ORIG_FILE$company) %like% "va"] <- "van houten"
REFINE_ORIG_FILE$company[tolower(REFINE_ORIG_FILE$company) %like% "un"] <- "unilever"

## 2: Separate product code and number
## Separate the product code and product number into separate columns i.e. 
## add two new columns called product_code and product_number, containing 
## the product code and number respectively
REFINE_ORIG_FILE$Product.code...number. <- as.character(REFINE_ORIG_FILE$Product.code...number.)
STR_SPLIT_PRODUCT <- strsplit(REFINE_ORIG_FILE$Product.code...number.,"-", fixed=TRUE)
STR_SPLIT_PRODUCT <- data.frame(STR_SPLIT_PRODUCT)
STR_SPLIT_PRODUCT <- transpose(STR_SPLIT_PRODUCT)
names(STR_SPLIT_PRODUCT) <- c("product code", "product number")
REFINE_ORIG_FILE <- cbind(STR_SPLIT_PRODUCT, REFINE_ORIG_FILE)
REFINE_ORIG_FILE <- REFINE_ORIG_FILE[ , !(names(REFINE_ORIG_FILE) %in% c("Product.code...number."))]
View(REFINE_ORIG_FILE)

## 3: Add product categories
## You learn that the product codes actually represent the following 
## product categories: p = Smartphone,v = TV,x = Laptop, q = Tablet
## In order to make the data more readable, add a column with the 
## product category for each record.
PROD_CAT <- REFINE_ORIG_FILE$`product code`
PROD_CAT <- data.frame(PROD_CAT)
names(PROD_CAT) <- "product category"
PROD_CAT$`product category` <- as.character(PROD_CAT$`product category`)
PROD_CAT$`product category`[PROD_CAT$`product category` %like% "p"] <- "Smartphone"
PROD_CAT$`product category`[PROD_CAT$`product category` %like% "v"] <- "TV"
PROD_CAT$`product category`[PROD_CAT$`product category` %like% "x"] <- "Laptop"
PROD_CAT$`product category`[PROD_CAT$`product category` %like% "q"] <- "Tablet"
REFINE_ORIG_FILE <- cbind(PROD_CAT, REFINE_ORIG_FILE)

## 4: Add full address for geocoding
## You'd like to view the customer information on a map. 
## In order to do that, the addresses need to be in a form that 
## can be easily geocoded. Create a new column full_address that 
## concatenates the three address fields (address, city, country), 
## separated by commas.
FULL_ADDR <- paste(REFINE_ORIG_FILE$address, REFINE_ORIG_FILE$city, REFINE_ORIG_FILE$country, sep=", ")
FULL_ADDR <- data.frame(FULL_ADDR)
names(FULL_ADDR) <- "full_address"
REFINE_ORIG_FILE <- cbind(REFINE_ORIG_FILE, FULL_ADDR)

## 5: Create dummy variables for company and product category
## Both the company name and product category are categorical variables 
## i.e. they take only a fixed set of values. In order to use them in 
## further analysis you need to create dummy variables. 
## Create dummy binary variables for each of them with the prefix 
## company_ and product_ i.e.
## Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
COMPANY_BINARY <- data.frame(company_phillips=1:25, company_akzo=1:25, company_van_houten=1:25, company_unilever=1:25)
COMPANY_BINARY[COMPANY_BINARY>0] <-0
## Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet
PRODUCT_BINARY <- data.frame(product_smartphone=1:25, product_tv=1:25, product_laptop=1:25, product_tablet=1:25)
PRODUCT_BINARY[PRODUCT_BINARY>0] <-0
REFINE_ORIG_FILE <- cbind(REFINE_ORIG_FILE, COMPANY_BINARY, PRODUCT_BINARY)
## Mapping of company boolean
REFINE_ORIG_FILE$company_phillips[REFINE_ORIG_FILE$company %like% "phil"] <- 1
REFINE_ORIG_FILE$company_akzo[REFINE_ORIG_FILE$company %like% "ak"] <- 1
REFINE_ORIG_FILE$company_van_houten[REFINE_ORIG_FILE$company %like% "van"] <- 1
REFINE_ORIG_FILE$company_unilever[REFINE_ORIG_FILE$company %like% "uni"] <- 1

## Mapping of product boolean
REFINE_ORIG_FILE$product_smartphone[REFINE_ORIG_FILE$`product category` %like% "Smartphone"] <- 1
REFINE_ORIG_FILE$product_laptop[REFINE_ORIG_FILE$`product category` %like% "Laptop"] <- 1
REFINE_ORIG_FILE$product_tv[REFINE_ORIG_FILE$`product category` %like% "TV"] <- 1
REFINE_ORIG_FILE$product_tablet[REFINE_ORIG_FILE$`product category` %like% "Tablet"] <- 1
write.csv(REFINE_ORIG_FILE, file="refine_clean.csv")

## 6: Submit the project on Github
## Include your code, the original data as a CSV file refine_original.csv, 
## and the cleaned up data as a CSV file called refine_clean.csv