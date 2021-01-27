

#Setting working directory

setwd("D://Reliance Case Study")

# call libraries which will be reuired to do the analysis and also use option to dislay regular numbers instead of exponentional terms
#The central package is tidyverse, which contains tidyr, dplyr, ggplot, etc. Other packages I am using are tidyquant for its nice ggplot theme,
#modelr, gridExtra and grid for additional plotting functionalities.

library(dplyr)
library(graphics)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(modelr)
library(gridExtra)
library(grid)
library(lubridate)
options(scipen = 999)


#Read CSV Data

Original.data <- read.csv("Data Science CaseStudy Data.csv",header = TRUE,stringsAsFactors = FALSE,fileEncoding="UTF-8-BOM")

### Total 51290 records in the original data with 9 varibales. No predefine target varible for the buisness use case.

# summary statistics and structure of the data

summary(Original.data)

str(Original.data)

### As per findings, "ORDERDATE" and "DELIVERYDATE" are shown as charcter data tyoe so need to be converterd into Date data type for analysis.

#Find out missing value in dataset

colSums(is.na(Original.data))

### No missing vlaues in the provided data for analysis

# covert the dates columns into date format

Original.data$ORDER.DATE <- dmy_hms(Original.data$ORDERDATE)

Original.data$DELIVERY.DATE <- dmy_hms(Original.data$DELIVERYDATE)

class(Original.data$ORDER.DATE)



### "ORDER.DATE" and "DELIVERY.DATE" both converted into the date format. Not using the time information as that is not required for our analysis becuase 
###it just shows the time which doesnt provide any useful information for analysis.


# Data type recheck 

str(Original.data)

# Data Sanity Check post conversion

summary(Original.data)

### Post conversion into date format we found that Order.Date varibale has 24 missing vlaues and DELIVERY.DATE has 76 missing vlaues while original 
### data dont have any missing values for these original fields.

# Find the records which have missing ORDER.DATE and DELIVERY.DATE

Order.date.missing <- Original.data[is.na(Original.data$ORDER.DATE),]

Delivery.date.missing <- Original.data[is.na(Original.data$DELIVERY.DATE),]

# find out the unique vlaues in ORDERDATE and DELIVERYDATE so that we can see which dates are creating missing vlaue issues post format conversion.

unique(Order.date.missing$ORDERDATE)

unique(Delivery.date.missing$DELIVERYDATE)

### While checking the output it shows all the records which are having ORDERDATE and DELIVERYDATE as "29-FEB-2017 12.00.00.000000000 AM" created the 
### missing vlaues issues.while investing the problem further it was found that Feb 2017 is actually a month of 28 days not 29 days month. So this is a 
### case of data extraction mistakes done during the data gathering excercise


#Extracting Variable names

var.name <-as.data.frame(names(Original.data))


#Recording Data Type for each Variable
var.name$DataType<-sapply(Original.data,class)




#No. of Records for each Variable
var.name$No.ofRecords<-nrow(Original.data)

#Counting No. of Unique Values for each variable
for(i in 1:ncol(Original.data))
{
  var.name$UniqueRecords[i]<-length(unique(Original.data[,i]))
}


#No.of observations available for each variable and its percentage
var.name$DataAvailable<-colSums(!is.na(Original.data))
var.name$AvailablePercentage<-round(colMeans(!is.na(Original.data)),4)


#Total and Percentage of Missing Values for each Variable
var.name$Missing<-colSums(is.na(Original.data))
var.name$MissingPercentage<-round(colMeans(is.na(Original.data)),4)

### while checking the data quality report(var.name) we observed that total no of unique records in ORDERNO and INVOICENO are 10428 and 25385 repectively out 
### of 51290 total no of records. it means that same order will be delivered in patches as both varibles dont have missing vlaues. While cross checking
### the same this assumption holds true & also the total no of different iteams ( products) are 528. Hence the iteam code and ORDERNO will create a unique
### identifier to capture any unique record and provide the visibility on the product level which is critical as we need to find out the top products and 
### the orders of such products for our analysis.

class(Original.data$ORDERDATE)

# Treating problemitic dates  

Original.data$DELIVERYDATE <- ifelse (Original.data$DELIVERYDATE =="29-FEB-2017 12.00.00.000000000 AM","28-FEB-2017 12.00.00.000000000 AM", Original.data$DELIVERYDATE)

Original.data$ORDERDATE <- ifelse (Original.data$ORDERDATE =="29-FEB-2017 12.00.00.000000000 AM","28-FEB-2017 12.00.00.000000000 AM", Original.data$ORDERDATE)

### Date problem has been fixed

# Now recreate the order.date and delivery.date as Date class Type

Original.data$ORDERDATE <- as.Date(dmy_hms(Original.data$ORDERDATE))

Original.data$DELIVERYDATE <-as.Date(dmy_hms(Original.data$DELIVERYDATE))

# now check for missing vlaue imputaion result to find out still have missing date problem exsists or not

summary(Original.data)

### No mising value in the modified date columns as observered previously.

### As per the above observations we need to group the records on the basis of ORDERNO and ITEMCODE. Hence we dont required the DELIVERY.DATE varibles in our 
### analysis as this date is not giving any useful information for analysis.

# creating a new varible which will work as unique key to our further analysis.

Original.data$order.key <- as.character(paste0(Original.data$ITEMCODE,Original.data$ORDERNO))

# as the delivery date is just to enure when the product is delivered and no rleation in terms of inventry management so we can ignore this variable
# from our analysis and hence the delivery date and invoice number from our analysis.As we dont have the customer demographic information so only the 
# cusomter id will not provide any usefull information for analysis so we can igonre this field from the analysis.
# Derived varible to find out the no of shortfall against each order.

Original.data$stockmeasure <- (Original.data$DELIVEREDQUANTITY - (Original.data$OREDEREDQUANTITY))

Original.data$Gain.Loss <- ifelse(Original.data$OREDEREDQUANTITY >0,"Gain","Loss")

Original.data$Sotckshortage <- ifelse(Original.data$stockmeasure <0,"Yes","No")

# Creating Extra features from the "ORDERDATE" for our analysis

Original.data$Order.Year <- as.Date(cut(Original.data$ORDERDATE,breaks = "year"))

Original.data$day <- parse_date(format(Original.data$ORDERDATE, "%Y-%m-%d"))

Original.data$Order.Month <- as.Date(cut(Original.data$ORDER.DATE,breaks = "month"))

Original.data$day_of_week <- wday(parse_date(format(Original.data$ORDER.DATE), "%Y-%m-%d"),label=TRUE)


# remove rows where the column is duplicated so that every order placed will give the correct no of products which has been ordered as same order was
# delivered in multiple time and every such records have the original counts of products which was placed first time instead of the delta remaining


analsyis.data <- Original.data %>% distinct(order.key, .keep_all = TRUE)


str(analsyis.data)

### post removal of duplicate records, overall 34596 records left out of 51290 records.

analsyis.data <- analsyis.data[,-c(5,8,10:12)]

str(analsyis.data)


#.......................................................Exploratory Analysis of the given data set........................................................

## find the items which have shortfall to fullfill the demand ( Years wise /  monthly)

# Analysis on Txns over the time................. 

Txns <- analsyis.data %>% ggplot(aes(x = day, color = Gain.Loss)) + facet_grid(Gain.Loss ~ ., scales = "free") +
                          geom_freqpoly(bins = 100, size = 1, alpha = 0.8) + scale_color_manual(values = palette_light()) +
                          theme_tq() + guides(color = FALSE) + labs(title = "Number of purchases and returns over Years",x ="")
 
### During the first quater of the years the sale is more compaired to rest of the months. No of purchase is higher than the number of products returns during the given time period.
 
# Analysis on Items..............................
  
#items that are being purchases or returned. Here, we sum up the net quantities for each item. 
 
Iteam.performace<- analsyis.data %>% group_by(ITEMCODE) %>% summarise(sum = sum(OREDEREDQUANTITY)) %>% arrange(-sum)

### # A tibble: 528 x 2
###ITEMCODE       sum
###<chr>        <dbl>
### 1 IT0001012 2939031.
### 2 IT0001076 1950562.
### 3 IT0001088 1472332.
### 4 IT0001020 1038495.
### 5 IT0001293  752000.
### 6 IT0001096  570999.
### 7 IT0001084  376290.
### 8 IT0001016  316542.
### 9 IT0001265  250350.
### 10 IT0001307  229746.
# ... with 518 more rows
  
#As we can see in the plots below, the majority of items is purchases only occasionally, while a few items are purchased a lot.

  p1 <- analsyis.data %>%
    group_by(ITEMCODE) %>%
    summarise(sum = sum(OREDEREDQUANTITY)) %>%
    ggplot(aes(x = sum)) +
    geom_density(fill = palette_light()[[1]], alpha = 0.8) +
    theme_tq()
  
  p2 <- analsyis.data %>%
    group_by(ITEMCODE) %>%
    summarise(sum = sum(OREDEREDQUANTITY)) %>%
    filter(sum > 423) %>%
    ggplot(aes(x = sum)) +
    geom_density(fill = palette_light()[[1]], alpha = 0.8) +
    theme_tq()
  
  p3 <- analsyis.data %>%
    group_by(ITEMCODE) %>%
    summarise(sum = sum(OREDEREDQUANTITY)) %>%
    filter(sum > 25000) %>%
    ggplot(aes(x = sum)) +
    geom_density(fill = palette_light()[[1]], alpha = 0.8) +
    theme_tq()
  
grid.arrange(p1, p2, p3, ncol = 3)
  
  
### the majority of items is purchases only occasionally, while a few items are purchased a lot
  
  
# most demanding products during the years. In terms of how many different days items has been purchased by the customers
  
most_demanding_product <- analsyis.data %>%
    group_by(day, ITEMCODE) %>%
    summarise(sum = sum(OREDEREDQUANTITY)) %>%
    group_by(ITEMCODE) %>%
    summarise(n = n()) %>%
    arrange(-n)
  
head(most_demanding_product)

### A tibble: 6 x 2
### ITEMCODE      n
###  <chr>     <int>
###1 IT0001088  1083
###2 IT0001076  1077
###3 IT0001012  1000
###4 IT0001020   947
###5 IT0001151   873
###6 IT0001113   750  
### IT0001088,IT0001076,IT0001012,IT0001020,IT0001151 are the top 5 most demanding products purchased by the customers

# The item that has been purchased most often in terms of days is the IT0001088. Let's look at its distribution of sold/returned quantities per day 
  
analsyis.data %>% filter(ITEMCODE == "IT0001088") %>% group_by(day, Gain.Loss) %>% summarise(sum = sum(OREDEREDQUANTITY)) %>%
                  ggplot(aes(x = day, y = sum, color = Gain.Loss)) + facet_wrap(~ Gain.Loss, ncol = 1, scales = "free") +
                  geom_line(size = 1, alpha = 0.5) + scale_color_manual(values = palette_light()) + theme_tq() +
                  labs(x = "", y = "sum of quantities", color = "", title = "Transactions of IT0001088")
  
# Analysis of customers...................................
  
# Transaction made by cutomer
  
analsyis.data %>%
    ggplot(aes(x = CUSTOMER, fill = Gain.Loss)) +
    geom_bar(alpha = 0.8) +
    scale_fill_manual(values = palette_light()) +
    theme_tq() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "",
         fill = "")
  
 
# Top Customers according to the ordered quantity
  
top_cust <- analsyis.data %>% group_by(ITEMCODE, CUSTOMER) %>% summarise(sum = sum(OREDEREDQUANTITY)) %>% group_by(CUSTOMER) %>%
                          summarise(n = n()) %>% arrange(-n)
  
head(top_cust)
  
### A tibble: 6 x 2
###   CUSTOMER     n
###     <chr>    <int>
###  1 C00224      74
###  2 C00143      70
###  3 C00245      66
###  4 C00313      57
###  5 C00315      57
###  6 C00335      56
  
# Pattern of customers
  
hist(top_cust$n)
  
### Most of the customers (>150) made the transations less than a 10.Very few customers has done done the buiness between 70 to 80 transactions.
  
# Repeat cutomers identification to improve the company customer relationship
  
rep_customer <- analsyis.data %>% group_by(day, CUSTOMER) %>% summarise(sum = sum(OREDEREDQUANTITY)) %>%
                                  group_by(CUSTOMER) %>% summarise(n = n()) %>%
                                  mutate(repeat_customer = ifelse(n > 1, "repeat_cust", "one_time_cust"))

# find no of repate customeers

length(which(rep_customer$repeat_customer == "repeat_cust"))
  
### overall 215 cutomers are repeated customers  
  
rep_customer_day <- left_join(analsyis.data, rep_customer, by = "CUSTOMER") %>% distinct(day, CUSTOMER, repeat_customer) %>%
                    group_by(day, repeat_customer) %>% summarise(n = n()) %>% spread(key = repeat_customer, value = n)

### On 2017-08-04 maximum customers placed the orders which is 21

# Proporation of one time customer and repeat customers
  
rep_customer %>% group_by(repeat_customer) %>% summarise(n = n()) %>% mutate(prop = n / sum(n)) %>%
                ggplot(aes(x = "", y = prop, fill = repeat_customer)) + geom_bar(stat = "identity", alpha = 0.8) +
                coord_polar("y", start = 0) + scale_fill_manual(values = palette_light()) + theme_tq() + theme(legend.position = "right") +
                labs(x = "", y = "", fill = "", title = "Proportion of one-time & repeat customers")
  
# Transactions, quantities and items per customer and day
  
# mean net quantities of products (mean_quant_cus) from all customers per day, mean number of products (mean_items_cust) from all customers per day
  
customer_purch <- analsyis.data %>% group_by(day, CUSTOMER) %>% summarise(n = n(), sum_it = sum(OREDEREDQUANTITY)) %>%
                                    group_by(day) %>% summarise(mean_quant_cust = mean(sum_it),mean_items_cust = mean(n))
  
head(customer_purch)

### A tibble: 6 x 3
### day        mean_quant_cust mean_items_cust
### <date>               <dbl>           <dbl>
###   1 2013-09-05            400.            1.00
### 2 2013-09-22           3184.            3.50
### 3 2013-09-23           7464.            3.00
### 4 2013-09-26            288.            1.00
### 5 2013-10-02            392.            2.00
### 6 2013-10-06           2421.            8.50
  
customer_purch %>% gather(x, y, mean_quant_cust:mean_items_cust) %>% 
                    ggplot(aes(x = day, y = y)) + facet_wrap(~ x, ncol = 1, scales = "free") +
                    geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) + 
                    geom_smooth(color = palette_light()[[2]], method = 'loess') + theme_tq() + labs(x = "", y = "")
  
# Purchases/returns per day
  
Gain.Loss <- analsyis.data %>% group_by(day, Gain.Loss) %>% summarise(sum = sum(OREDEREDQUANTITY)) %>% spread(key = Gain.Loss, value = sum)
  
  
Gain.Loss %>% gather(x, y, Gain:Loss) %>%
              ggplot(aes(x = day, y = y, color = x)) +
              geom_line(size = 1, alpha = 0.8) +
              scale_color_manual(values = palette_light()) + theme_tq() + labs(x = "", y = "quantity of items",color = "")
  
# Finding how many different items are purchased/returned per day
  
  n_items <- analsyis.data %>% group_by(day, ITEMCODE) %>% summarise(n = n()) %>%
                                group_by(day) %>% summarise(n_items = n())
  
  n_items %>% ggplot(aes(x = day, y = n_items)) +
              geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
              geom_smooth(color = palette_light()[[2]], method = 'loess') +
              theme_tq() + labs(x = "", y = "number of different items", color = "")

### No of different items purchased is increasing over the time
  
# Total no of units sold per day
  
  No.of.units <- analsyis.data %>% group_by(day) %>% summarise(sum_quantity = sum(OREDEREDQUANTITY),mean_quantity = mean(OREDEREDQUANTITY))
  
  No.of.units %>% gather(x, y, sum_quantity:mean_quantity) %>%
                  ggplot(aes(x = day, y = y)) +
                  facet_wrap(~ x, ncol = 1, scales = "free") +
                  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
                  geom_smooth(color = palette_light()[[2]], method = 'loess') +
                  theme_tq() + labs(x = "", y = "")
  
  
# Return analysis.....................................
  
returns <- analsyis.data %>% filter(OREDEREDQUANTITY < 0) %>% group_by(day) %>% summarise(sum_quantity_return = sum(OREDEREDQUANTITY),
              mean_quantity_return = mean(OREDEREDQUANTITY))
  
head(returns)
  
### A tibble: 6 x 3
###   day        sum_quantity_return mean_quantity_return
###   <date>                   <dbl>                <dbl>
### 1 2013-12-26               -992.               -90.2 
### 2 2014-01-23                 -6.                -6.00
### 3 2014-02-06               -834.               -46.3 
### 4 2014-02-09              -1149.              -144.  
### 5 2014-02-25              -1324.              -265.  
### 6 2014-03-19               -125.              -125
  
returns %>% gather(x, y, sum_quantity_return:mean_quantity_return) %>% 
            ggplot(aes(x = day, y = y)) + 
            facet_wrap(~ x, ncol = 1, scales = "free") + 
            geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) + theme_tq() + labs(x = "", y = "")
  

# Most sold items
  
most_sold_day <- analsyis.data %>% filter(ITEMCODE %in% most_demanding_product$ITEMCODE[1:5]) %>% group_by(day, ITEMCODE) %>%
                                  summarise(sum = sum(OREDEREDQUANTITY)) %>% spread(key = ITEMCODE, value = sum)
    
  
analsyis.data %>% filter(ITEMCODE %in% most_demanding_product$ITEMCODE[1:5]) %>% group_by(day, ITEMCODE) %>%
                        summarise(sum = sum(OREDEREDQUANTITY)) %>%
                        ggplot(aes(x = day, y = sum)) + facet_wrap(~ ITEMCODE, ncol = 1, scales = "free") +
                        geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
                        theme_tq() + labs(x = "", y = "net sum of quantites sold")


# Creating Data Frame for Modelling post extracting the daywise features to implement the Time Series Model

Business.per.day <- distinct(select(analsyis.data, day, day_of_week, Order.Month)) %>%
                    left_join(No.of.units, by = "day") %>%
                    left_join(customer_purch, by = "day") %>%
                    left_join(most_sold_day, by = "day") %>%
                    left_join(n_items, by = "day") %>%
                    mutate(diff_sum_quantity = sum_quantity - lag(sum_quantity),
                    season = ifelse(Order.Month %in% c("04", "05", "06"), "summer",
                                    ifelse(Order.Month %in% c("07", "08", "09"), "Monsoon", 
                                    ifelse(Order.Month %in% c("10", "11"), "autumn ", "winter"))))
  
  
Business.per.day %>% # remove last day because it is so extreme
                  filter(day != max(Business.per.day$day)) %>%
                  gather(x, y, sum_quantity:diff_sum_quantity) %>%
                  ggplot(aes(x = Order.Month, y = y)) +
                  facet_wrap(~ x, scales = "free", ncol = 2) +
                  geom_line(alpha = 0.8, color = palette_light()[[1]]) +
                  geom_point(aes(color = day_of_week)) +
                  geom_smooth() +
                  scale_color_manual(values = palette_light()) +
                  theme_tq() +
                  labs(x = "",
                       y = "",
                       color = "day of the week")

# creating Training and Testing Datset


Business.per.day <- Business.per.day %>% mutate(model = ifelse(day <= "2017-06-01", "train", "test"))

Business.per.day %>% ggplot(aes(x = day, y = sum_quantity, color = model)) + geom_point(alpha = 0.5) + geom_line(alpha = 0.5) +
                        scale_color_manual(values = palette_light()) + theme_tq()




train <- filter(Business.per.day, model == "train") %>% select(-model)

test <- filter(Business.per.day, model == "test")

# Model Bulinding using Time Series Machine Algo

