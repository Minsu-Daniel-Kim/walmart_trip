library(ggplot2)
library(data.table)
library(dplyr)

train <- fread("train.csv")
str(train)

# TripType
# VisitNumber
# Weekday
# Upc
# ScanCount
# DepartmentDescription
# FinelineNumber

train$Weekday <- factor(train$Weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday", "Friday", "Saturday", "Sunday"))
train$TripType <- as.factor(train$TripType)


# Distribution of TripType, Weekday, ScanCount, and DepartmentDescription
ggplot(train, aes(x = as.factor(TripType))) + geom_bar() + xlab("Trip Type")
ggplot(train, aes(x = as.factor(Weekday))) + geom_bar()
ggplot(train, aes(x = as.factor(ScanCount))) + geom_bar()
ggplot(train, aes(x = as.factor(DepartmentDescription))) + geom_bar()


# TripTypes are 3~9, 12, 14, 15, 18~43, 999 where 999 is other

# Looking at type 40
type40 <- filter(train, TripType == 40)
type40 %>% 
  ggplot(aes(x = Weekday, fill = Weekday)) + geom_bar()
type40 %>%
  ggplot(aes(x = DepartmentDescription, fill = DepartmentDescription)) + geom_bar()
max(table(type40$DepartmentDescription))


# women-ish. Trying to isolate ladies
# wom <- c("LADIESWEAR", "SHEER HOSIERY", "BATH AND SHOWER", "LADIES SOCKS", "BEAUTY")
wom <- c("LADIESWEAR", "SHEER HOSIERY", "LADIES SOCKS")
# wom <- c("MENS WEAR")
women <- filter(train, DepartmentDescription %in% wom)
ggplot(women, aes(x = TripType, fill = TripType)) + geom_bar()
ggplot(women, aes(x = Weekday, fill = Weekday)) + geom_bar()
ggplot(women, aes(x = as.factor(ScanCount), fill = ScanCount)) + geom_bar()

ggplot(train, aes(x = Weekday, y = as.factor(TripType), colour = Weekday)) + geom_point()
ggplot(train, aes(x = as.factor(ScanCount), y = as.factor(TripType))) + geom_jitter()
type25 <- filter(train, TripType == 25)
ggplot(type25, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





