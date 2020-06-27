# clear the environment
rm(list=ls(all=TRUE))

# load data
library(rio)
inequality_data <- import("inequality.xlsx")
View(inequality_data)

# cross sectional data set 
head(inequality_data)
summary(inequality_data)

# subset inequality gini for Denmark and Sweden
inequality_sub = subset(inequality_data, select = c("country", "inequality_gini"))
inequality_subfilter = filter(inequality_sub, country == "Denmark") 
inequality_subfilter2 = filter(inequality_sub, country == "Sweden")
view(inequality_subfilter)
view(inequality_subfilter2)

# Brazil 
inequality_sub = subset(inequality_data, select = c("country", "inequality_gini"))
inequality_subfilter3 = filter(inequality_sub, country == "Brazil") 
view(inequality_subfilter3)

# Get a quick peak at data frame using head command
head(inequality_sub)

# Define function to remove accents
remove.accents <- function(s) {
  
  #1 character substitutions
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1,new1,s)
  
}

# Remove accents/apply function 
inequality_data$country <- remove.accents(inequality_data$country)

# Check if removed 
head(inequality_data)

# Sort data
inequality_data = inequality_data[order(inequality_data$inequality_gini),]
inequality_sub = inequality_sub[order(inequality_data$inequality_gini),]

# run head again to see top 5 countries
head(inequality_data)
head(inequality_sub)

# mean inequality_gini score
summary(inequality_data)
mean(inequality_data$inequality_gini)
summary(inequality_sub)

# Create dummy variable
inequality_data$inequality_gini = NA
inequality_data$inequality_gini[inequality_data$inequality_gini <= 36.81] = 1
inequality_data$inequality_gini[inequality_data$inequality_gini > 36.81] = 0

for (r in 1:nrow(inequality_sub))
  if(inequality_sub[r,2] == 1) {
    print('low_inequality')
  } else {
    print('high_inequality')
}

 view(inequality_sub)

inequality_data$inequality_gini[inequality_data$inequality_gini == 1] = "low_inequality"
inequality_data$inequality_gini[inequality_data$inequality_gini == 0] = "high_inequality"

# cross tab
library(doBy)
summaryBy(inequality_gini ~ "low_inequality", data=inequality_data, FUN=c(mean, length))

# create an organization vector
orgs = c('World Bank','African Development Bank', 'Bill and Melinda Gates Foundation')

# create for loop 
for (i in orgs){
  print(i)
}

## adding world development indicator data directly from r
install.packages("WDI")

# add some data from the world development Indicators (WDI)
# income share held by highest 10% 
# import data 
library (WDI)
highestincome_data = WDI(country = "all",
                      indicator = ("SI.DST.10TH.10"),
                      start = 2015, end = 2015, extra = FALSE, cache = NULL)

# see data 
summary(highestincome_data)

# renaming variables 
library(data.table)
setnames(highestincome_data, "SI.DST.10TH.10", "Income Share by Highest 10%")

# merging data
merged_df = left_join(x = inequality_data,
                        y = highestincome_data, 
                        by = c("country", "year"))

# remove NA
#check for the NAs
table(merged_df$inequality_data, exclude = TRUE)
table(merged_df$highestincome_data, exclude =TRUE)
subset(merged_df, is.na(inequality_data))
subset(merged_df, is.na(highestincome_data))
#actually that didn't work so
data_no_NAS = na.omit(merged_df, select =c("inequality_data", "highestincome_data"))

# filtering and piping 
library(tidyverse)
data_greater_30 <-
  inequality_data %>%
  dplyr::filter(!(inequality_gini > 30)) 

data_greater_30 <-
  data_NO_NAS %>%
  dplyr::filter(!(inequality_gini > 30)) 


# look for ai 
grep("ai", data_greater_30)

# apply 
matrix1 = matrix(C<-(1:17),nrow=17, ncol=4)
matrix1_a = apply(matrix1, 2, sum)

matrix <- matrix(c(1:17), ncol = 4, bycol = TRUE)
print(Matrix)

sum_gini = `lapply(data_no_NAS, sum(data_no_NAS$inequality_gini))`
str(sum_gini)

# label data 
library(labelled)
var_label(data_no_NAS) <- list(`country` = "country",
                               `year` = "year",
                               `inequality_gini` = "Inequality Gini",
                               `iso2c` = "iso-2 country code")

# save the data frame as stata
library(rio)
export(data_no_NAS, "final_data.dta")


