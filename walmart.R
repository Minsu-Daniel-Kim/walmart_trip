library(ggplot2)
library(data.table)

train <- read.csv("train.csv")
str(train)

ggplot(train, aes(x = as.factor(TripType))) + geom_bar()
ggplot(train, aes(x = as.factor(Weekday))) + geom_bar()
ggplot(train, aes(x = as.factor(ScanCount))) + geom_bar()
ggplot(train, aes(x = as.factor(DepartmentDescription))) + geom_bar()

ggplot(train, aes(x = Weekday, y = as.factor(TripType), colour = Weekday)) + geom_point()
ggplot(train, aes(x = as.factor(ScanCount), y = as.factor(TripType))) + geom_jitter()

ggplot(train, aes(x = as.factor(Weekday), y = as.factor(TripType), color = Weekday)) + geom_point()

ggplot(train, aes(x = FinelineNumber)) + geom_bar()
ggplot(train, aes(x = as.factor(Upc))) + geom_bar()
