library(ggplot2)
library(data.table)
library(dplyr)

train <- fread("train.csv")
str(train)

train$Weekday <- factor(train$Weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday", "Friday", "Saturday", "Sunday"))


# Distribution of TripType, Weekday, ScanCount, and DepartmentDescription
ggplot(train, aes(x = as.factor(TripType))) + geom_bar()
ggplot(train, aes(x = as.factor(Weekday))) + geom_bar()
ggplot(train, aes(x = as.factor(ScanCount))) + geom_bar()
ggplot(train, aes(x = as.factor(DepartmentDescription))) + geom_bar()


train %>% 
  filter(TripType == 40) %>%
  ggplot(aes(x = Weekday)) + geom_bar()

ggplot(train, aes(x = Weekday, y = as.factor(TripType), colour = Weekday)) + geom_point()
ggplot(train, aes(x = as.factor(ScanCount), y = as.factor(TripType))) + geom_jitter()

ggplot(train, aes(x = as.factor(Weekday), y = as.factor(TripType), color = Weekday)) + geom_point()

ggplot(train, aes(x = FinelineNumber)) + geom_bar()
ggplot(train, aes(x = as.factor(Upc))) + geom_bar()
