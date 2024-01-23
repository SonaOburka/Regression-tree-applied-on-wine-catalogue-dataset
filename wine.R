library("tidyverse")
library("dplyr")
library("rvest")
library("stringr")
library("glue") # patri k balicku trim 
library(base)
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

setwd("C:/Users/GitHub")
wine <- read.csv("wine_data_sample.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

#----------------------------
# 1. FIRST LOOK INTO RAW DATA
#----------------------------

#-----glance at what data frame "wine" looks like-----
head(wine,10)
str(wine)
sapply(wine, class)
#-----glance at descriptive statistics of the raw data as well as number of NA values and empty strings-----
summary(wine)
colSums(is.na(wine))
colSums(wine == "", na.rm = TRUE)

# 2.DATASET CLEANING

#-----removal of duplicated records-----
# number of duplicate rows
nrow(wine) - nrow(unique(wine))
# removal of duplicate records
wine <- unique(wine)
# number of rows in wine dataset
nrow(wine)

#-----variable "wine"-----
# variable "province" extracted from the variable "wine" and converted to data type factor 
wine$province <- sub("\\)", "",sub("[[:blank:]]\\(", "",str_extract(wine$wine, "[[:blank:]]\\(.*")))
wine$province <- as.factor(wine$province)
# variable "year" extracted from the variable "wine" and converted to data type factor 
wine$year <- str_extract(wine$wine, "2[0-9]{3}|19[0-9]{2}(?!([A-z\\s]*$))")  
wine$year <- as.factor(wine$year)

#-----variable winery-----
# variable "winery" converted to data type factor 
wine$winery <- as.factor(wine$winery)

#-----variable category-----
# variable "category" converted to data type factor 
wine$category <- as.factor(wine$category)

#-----variable designation-----
# empty strings replaced with NA and converted to data type factor
wine$designation <- ifelse(wine$designation == "", NA, wine$designation)
wine$designation <- as.factor(wine$designation)

#-----variable varietal-----
# empty strings replaced with NA and converted to data type factor
wine$varietal <- ifelse(wine$varietal == "", NA, wine$varietal)
wine$varietal <- as.factor(wine$varietal)

#-----variable appellation-----
# variable "country" extracted from the column "appellation" and converted to data type factor 
wine <- wine |> 
  mutate(country = str_extract(appellation, "(?<=(,\\s))[A-z\\s]*$"))
wine$country <- as.factor(wine$country)
# variable "region" extracted from the column "appellation", its empty strings were removed and variable was converted to data type factor
wine$region <- sub("^\\s","", str_extract(wine$appellation,"([[[:alnum:]]\\s]*)(?=,\\s[A-z\\s]*$)"))
wine$region <- ifelse(wine$region == "", NA, wine$region)
wine$region <- as.factor(wine$region)
# empty strings replaced with NA and converted to data type factor
wine$appellation <- ifelse(wine$appellation == "", NA, wine$appellation)
wine$appellation <- as.factor(wine$appellation)

#-----variable alcohol-----
# character % removed from the column alcohol and conversion of the variable to numeric data type
wine$alcohol <- sub("%", "", wine$alcohol)
wine$alcohol <- as.numeric(wine$alcohol)
# removed records having the alcohol value over 22 % (which is the highest alcohol % that has e.g. Sherry) and renaming the variable as alcohol_pct
wine <- wine |> 
  filter(alcohol <= 22) |> 
  rename(alcohol_pct = alcohol)

#-----variable price-----
# character $ removed from the variable "price", conversion of the variable to numeric data type and renaming the variable price as price_usd
wine$price <- sub("\\$", "", wine$price)
wine$price <- as.numeric(wine$price)
# removed records having the value over 1.000 USD were removed as these seem to be error.
wine <- wine |> 
  filter(price <= 1000) |>   
  rename(price_usd = price)

#-----variable rating-----
# no action needed

#-----variable reviewer-----
# removal of empty strings
wine$reviewer <- ifelse(wine$reviewer == "", NA, wine$reviewer)

#-----variable review-----
# removal of empty strings
wine$review <- ifelse(wine$review == "", NA, wine$review)

#-----check number of NA and empty strings-----
colSums(is.na(wine))
colSums(wine == "", na.rm = TRUE)

#--------------------
# 3. OUTLIERS REMOVAL
#--------------------

#-----Check if outliers are present-----
# Boxplots of all continuous variables to distinguish outlier variables
par(mfrow = c(2,2))
boxplot(wine$alcohol_pct, xlab = "alcohol_pct", main="Alcohol in wines (in %)")
boxplot(wine$price_usd, xlab = "price_usd", main="Wine price (in USD)")   
boxplot(wine$rating, xlab = "rating", main="Wine rating (80-100)")
dev.off
# Variables saved in the data frame to be used for boxplots after outliers removal and the regression tree analysis 
wine_out <- wine[,c("alcohol_pct", "price_usd", "rating")]  

#-----Mahalanobis distance to remove outliers-----
wine_quant <- wine[,c("alcohol_pct", "price_usd", "rating")] 
wine_quant <- na.omit(wine_quant)

del_cumsum = 0
for (i in 1:20) {
    wine_quant <- wine[,c("alcohol_pct", "price_usd", "rating")] 
    mahal <- mahalanobis(wine_quant, 1/dim(wine_quant)[1]*t(wine_quant)%*%as.matrix(rep(1, dim(wine_quant)[1])), cov(wine_quant), inverted = FALSE)
    mahal_del <- length(mahal[mahal > 13])
    del_cumsum <- mahal_del + del_cumsum
    print(paste("Iteration:", i, "; Outlying records removed:",mahal_del, "; Cummulative sum of removed records:", del_cumsum))
    if (length(mahal[mahal > 13]) > 0){
      wine <- wine[-c(which(mahal > 13)),]
      wine_quant <- wine_quant[-c(which(mahal > 13)),]  
    } else break
}

#-------------------------------------------------------------------
# 4. DESCRIPTIVE STATISTICS AFTER THE DATASET CLEANING WAS COMPLETED
#-------------------------------------------------------------------

# Data for continuous variables boxplots  
alcohol_pct_boxplot <- list(With_outliers = wine_out$alcohol_pct,
                            Cleaned = wine_quant$alcohol_pct)
price_usd_boxplot <- list(With_outliers = wine_out$price_usd,
                          Cleaned = wine_quant$price_usd)
rating_boxplot <- list(With_outliers = wine_out$rating,
                       Cleaned = wine_quant$rating)
# Boxplots of continuous variables before and after outliers were removed in one graph 
par(mfrow = c(1,3))
boxplot(alcohol_pct_boxplot, xlab = "alcohol_pct", main="Alcohol in Wines (in %)")
boxplot(price_usd_boxplot, xlab = "price_usd", main="Wine Price (in USD)") 
boxplot(rating_boxplot, xlab = "rating", main="Wine Rating (80-100)")
dev.off

# by all variables
summary(wine)
colSums(is.na(wine))
colSums(wine == "", na.rm = TRUE)

#------------
# 5. ANALYSIS
#------------

#-----Dataset-----
wine_c <- wine |> 
  filter(country %in% c("Italy","France","Spain","US","Australia","Chile","Argentina","South Africa", "Germany", "Portugal")) |> 
  mutate(country_a = recode(country, Italy = "IT",
                            France = "FR",
                            Spain = "SP",
                            US = "US",
                            Australia = "AU",
                            Chile = "CL",
                            Argentina = "AR",
                            "South Africa" = "ZA",
                            Germany = "DE",
                            Portugal = "PT")
  ) |> 
  select(country_a, price_usd, category) 

#-----Libraries-----
library(rpart)
library(rpart.plot)
library(rattle)
#library(ggplot2)

#-----Regression Tree-----
# setting up train and test sets 
set.seed(1)
s = sample(c(TRUE, FALSE), nrow(wine_c), replace = TRUE, prob = c(0.8, 0.2))
train = wine_c[s,]
test = wine_c[!s,]
dim(train); dim(test)

# Regression tree
fit0 <- rpart(price_usd~.,data = train, cp = 0.001) 
fit0

fit <- rpart(price_usd~.,data = train, cp = 0.0000000001) 
fit

# Regression tree plot
par(mfrow = c(1,2))
fancyRpartPlot(fit0, main = "Regression Tree (cp = 0.001)", palettes = "Blues")
fancyRpartPlot(fit, main = "Regression Tree (cp = 0.0000000001)", cex = 0.15, palettes = "Blues")
dev.off

#-----Cross-validation-----
# table of "cp" and "xerror" values 
printcp(fit)
# vizualization of the relationship between "cp" and size of trees (tree nodes number) and "xerror". The minimum "xerror" is not visible.
plotcp(fit)
  # "cp" value corresponding with the min "xerror" value
cp_min <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
# prunning
fit <- prune(fit,cp_min)
fancyRpartPlot(fit, uniform=TRUE, palettes = "Blues", cex = 0.6)

#-----Model details-----
fit
summary(fit)
# count number of leaves
sum(fit$frame$var == "<leaf>")
# count number of final nodes
nrow(fit$frame)

#-----Prediction-----
# prediction of the price_usd applied on testing data
p = predict(fit,test) 
unique(p)

#-----MAPE score-----
MAPE <- mean(abs(test$price_usd - p)/test$price_usd) 

#----------------------------------------------------------------
#-----Analysis of reduced dataset (70 % of the original one)-----
#----------------------------------------------------------------

# setting up train and test sets 
set.seed(123)
#wine_s <- sample(wine, (nrow(wine))*0.7, replace = TRUE)
size <- (nrow(wine_c))*0.7
wine_s <- sample_n(wine_c, (nrow(wine_c))*0.7, replace = FALSE) 
s_small = sample(c(TRUE,FALSE), size, replace = TRUE, prob = c(0.8, 0.2))
train_s = wine_s[s_small,]
test_s = wine_s[!s_small,]
dim(train_s); dim(test_s)

# Regression tree
fit0_s <- rpart(price_usd~.,data = train_s, cp = 0.001) 
fit0_s

fit_s <- rpart(price_usd~.,data = train_s, cp = 0.0000000001) 
fit_s

# Regression tree plot
par(mfrow = c(1,2))
fancyRpartPlot(fit0_s, main = "Regression Tree - Reduced Dataset (cp = 0.001)", palettes = "Blues")
fancyRpartPlot(fit_s, main = "Regression Tree - Reduced Dataset (cp = 0.0000000001)", cex = 0.15, palettes = "Blues")
dev.off

#-----Cross-validation-----
# table of "cp" and "xerror" values 
printcp(fit_s)
# vizualization of the relationship between "cp", size of trees (tree nodes number) and "xerror". The minimum "xerror" is not visible.
plotcp(fit_s)
# "cp" value corresponding with the min "xerror" value
cp_min_s <- fit_s$cptable[which.min(fit_s$cptable[,"xerror"]),"CP"]
# prunning
fit_S <- prune(fit_s,cp_min_s)
fancyRpartPlot(fit_s, uniform=TRUE, cex = 0.6, palettes = "Blues")

#-----Model details-----
fit_s
summary(fit_s)
# count number of leaves
sum(fit_s$frame$var == "<leaf>")
# count number of final nodes
nrow(fit_s$frame)

#-----Prediction-----
# prediction of the price_usd applied on testing data
p_s = predict(fit_s,test) 
unique(p_s)

#-----MAPE score-----
MAPE_s <- mean(abs(test_s$price_usd - p_s)/test_s$price_usd) 
MAPE_s

#--------------
# 6. Min cp value and MAPE score 
#--------------

#-----original dataset-----
# min cp
cp_min
# MAPE score 
MAPE

#-----reduced dataset-----
# min cp
cp_min_s
# MAPE score 
MAPE_s

