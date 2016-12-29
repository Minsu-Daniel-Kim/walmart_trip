setwd("~/Desktop/kaggle/walmart_trip/")

library(dplyr)
library(data.table)
library(ggplot2)
source("taewook_util.R")

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




## Plotting 
### general
data <- get.dep.category(data)
ggplot(data, aes(TripType)) + geom_bar() + facet_wrap(~ Weekday)
ggplot(data, aes(TripType)) + geom_bar() + facet_wrap(~ weekend)
ggplot(data, aes(TripType)) + geom_bar(aes(y = (..count..)/sum(..count..))) + facet_wrap(~ Weekday)
ggplot(data, aes(TripType)) + geom_bar(aes(y = (..count..)/sum(..count..))) + facet_wrap(~ weekend)

ggplot(data, aes(TripType)) + geom_bar() + facet_wrap(~ DepartmentCategory)

ggplot(data, aes(FinelineNumber)) + geom_bar() + facet_wrap(~ weekend)
ggplot(data, aes(weekend)) + geom_bar()
ggplot(data, aes(Weekday)) + geom_bar() # by weekday frequency plot
ggplot(filter(data, as.integer(as.character(TripType)) <= 10), aes(DepartmentDescription, y = ..density..)) + geom_bar(aes(y = (..count..)/sum(..count..))) + facet_wrap(~TripType) 



### 요일별 판매되는 비율 (department-wise) / 어떤상품의 특성
data <- get.dep.category(data)
names(data)
ggplot(data, aes(DepartmentDescription)) + geom_bar(aes(y = (..count..)/sum(..count..)))  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) + 
  facet_wrap(~ Weekday , nrow = 3) +
  ggtitle("department sale density by weekday")

ggplot(data, aes(DepartmentCategory)) + geom_bar(aes(y = (..count..)/sum(..count..)))  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) + 
  facet_wrap(~ Weekday , nrow = 3) +
  ggtitle("department sale density by weekday")

df.dep <- data %>% group_by(Weekday, DepartmentDescription) %>% summarise(sum = sum(ScanCount), n = n())
df.tot <- data %>% group_by(Weekday) %>% summarise(sum_tot = sum(ScanCount), n_tot = n())
df.dep <- left_join(df.dep, df.tot, by = c("Weekday" = "Weekday"))
df.dep$percent_sum <- df.dep$sum / df.dep$sum_tot * 100
df.dep$percent_n <- df.dep$n / df.dep$n_tot * 100

# showing department description density by weekday
df.dep %>% arrange(DepartmentDescription) 

ggplot(df.dep) + geom_bar(aes(DepartmentDescription, percent_sum), stat= "identity") + facet_wrap(~ Weekday) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) +
  ggtitle("department sale density by weekday")

# sorting dep name by highest percentage of sales
df.dep.sort <- df.dep %>% group_by(DepartmentDescription) %>% summarise(perc = max(percent_sum)) %>% arrange(perc)
department.name <- unique(df.dep.sort$DepartmentDescription)

dep.1 <- df.dep %>% filter(DepartmentDescription %in% department.name[1:10])
dep.2 <- df.dep %>% filter(DepartmentDescription %in%department.name[11:20])
dep.3 <- df.dep %>% filter(DepartmentDescription %in%department.name[21:30])
dep.4 <- df.dep %>% filter(DepartmentDescription %in%department.name[31:40])
dep.5 <- df.dep %>% filter(DepartmentDescription %in%department.name[41:50])
dep.6 <- df.dep %>% filter(DepartmentDescription %in%department.name[51:60])
dep.7 <- df.dep %>% filter(DepartmentDescription %in%department.name[61:64])
dep.8 <- df.dep %>% filter(DepartmentDescription %in%department.name[65:68])

ggplot(dep.1) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.2) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.3) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.4) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.5) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.6) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.7) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")
ggplot(dep.8) + geom_bar(aes(Weekday, percent_sum), stat = "identity") + facet_wrap(~ DepartmentDescription, nrow = 2) + ggtitle("percent changes of department by weekday")

### department mean sd
df.dep.stat <- df.dep.scale %>% group_by(DepartmentDescription) %>% summarise(stdev = round(sd(percent_sum),5), Mean = round(mean(percent_sum),5)) %>% arrange(stdev) 


df.dep.scale <- df.dep[0,]
for (na in df.dep$Weekday) {
  test <- df.dep[df.dep$Weekday == na,]
  test$percent_sum <- scale(test$percent_sum)
  df.dep.scale <- rbind(df.dep.scale, test)
}







# want to cluster trip type based on how many items from different departments people buy 
library(tidyr)
df_fln_dd <- data %>% group_by(VisitNumber, FinelineNumber) %>% summarise(n=sum(ScanCount))
df_fln_dd_cd <- df_fln_dd %>% spread(key = FinelineNumber, value = n)


