setwd("~/Desktop/kaggle/walmart_trip/")

library(dplyr)
library(data.table)
library(ggplot2)

# Reading the data
data <- fread("train.csv")

# Looking at the structur of the data. Should integer number be factor? or vice versaa 
str(data)
sapply(data, class)
sapply(sapply(data, unique), length)
sort(unique(data$ScanCount))
summary(data$ScanCount)
sort(unique(data$DepartmentDescription))

# Setting data type into a more appropriate type
data$TripType <- factor(data$TripType)
data$Weekday <- factor(data$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday", "Saturday", "Sunday"))
data$Upc <- factor(data$Upc)
data$FinelineNumber <- factor(data$FinelineNumber)

# Basic Feature Engineering
data$weekend <- ifelse(data$Weekday %in% c("Saturday", "Sunday"), TRUE, FALSE)
data$return <- ifelse(data$ScanCount < -0, TRUE, FALSE)

# EDA 
data %>% group_by(TripType, DepartmentDescription) %>% summarise(n = n()) %>% View() # Variety of Department by each Trip Type
data %>% group_by(TripType, FinelineNumber) %>% summarise(n=n()) %>% View() 

df_sc_belowzero <- data %>% filter(ScanCount < 0)
df_tt_dd <- data %>% group_by(TripType, DepartmentDescription) %>% summarise(n = sum(ScanCount)) # trip type, 
df_fln_dd <- data %>% group_by(FinelineNumber, DepartmentDescription) %>% summarise(n=ScanCount)

df_sc_belowzero %>% group_by(TripType, DepartmentDescription) %>% summarise(n = n()) %>% View()

# want to cluster trip type based on how many items from different departments people buy 
library(tidyr)
df_fln_dd <- data %>% group_by(VisitNumber, FinelineNumber) %>% summarise(n=sum(ScanCount))
df_fln_dd_cd <- df_fln_dd %>% spread(key = FinelineNumber, value = n)



summary(data$ScanCount)





# Plotting 
ggplot(data, aes(weekend)) + geom_bar()
ggplot(data, aes(Weekday)) + geom_bar() # by weekday frequency plot
ggplot(filter(data, as.integer(as.character(TripType)) <= 10), aes(DepartmentDescription, y = ..density..)) + geom_bar(aes(y = (..count..)/sum(..count..))) + facet_wrap(~TripType) 



data %>% filter(VisitNumber == 255) %>% View()
data %>% filter(ScanCount < 0) %>% View()
data %>% filter(VisitNumber == 36866) %>% View()
36866
sort(unique(data$ScanCount))



## category 

unique(data$DepartmentDescription)
"MENSWEAR" "MENS WEAR" 
wear <- c("SHOES", "BOYS WEAR", "MENS WEAR" ,"LADIESWEAR","SHEER HOSIERY","LADIES SOCKS","GIRLS WEAR, 4-6X  AND 7-14","BRAS & SHAPEWEAR",
          "SLEEPWEAR/FOUNDATIONS","MENSWEAR","SWIMWEAR/OUTERWEAR","ACCESSORIES")
food <- c("DSD GROCERY", "MEAT - FRESH & FROZEN", "DAIRY", "PRODUCE","GROCERY DRY GOODS","FROZEN FOODS","SERVICE DELI","PRE PACKED DELI",
          "COMM BREAD","BAKERY","SEAFOOD")
medicine <- c("PHARMACY OTC","PHARMACY RX")
beauty <- c("PERSONAL CARE","BEAUTY","HEALTH AND BEAUTY AIDS")
luxury <- c("JEWELRY AND SUNGLASSES","OPTICAL - FRAMES","OPTICAL - LENSES")
electronics <- c("WIRELESS","ELECTRONICS","CAMERAS AND SUPPLIES","PLAYERS AND ELECTRONICS","MEDIA AND GAMING")
services <- c("FINANCIAL SERVICES","1-HR PHOTO")
household <- c("PETS AND SUPPLIES", "HOUSEHOLD CHEMICALS/SUPP", "HOME MANAGEMENT","COOK AND DINE","HOUSEHOLD PAPER GOODS","BEDDING",
               "BATH AND SHOWER","HOME DECOR","FURNITURE","LARGE HOUSEHOLD GOODS")
alcohol <- c("LIQUOR,WINE,BEER")
tool <- c("PAINT AND ACCESSORIES","AUTOMOTIVE","LAWN AND GARDEN")
other <- c("NULL",  "IMPULSE MERCHANDISE" ,"CANDY, TOBACCO, COOKIES","FABRICS AND CRAFTS" ,
           "CELEBRATION","HARDWARE","BOOKS AND MAGAZINES","OFFICE SUPPLIES","HORTICULTURE AND ACCESS",
           "SPORTING GOODS","OTHER DEPARTMENTS","SEASONAL","CONCEPT STORES")
baby <- c("INFANT CONSUMABLE HARDLINES","TOYS","INFANT APPAREL", "PLUS AND MATERNITY")






















library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
?createDataPartition
model.rpart <- rpart::rpart(as.factor(TripType) ~ ., data = data, method = "class")
fancyRpartPlot(fit)

