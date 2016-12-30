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
weekend <- c("Friday", "Saturday", "Sunday")
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday")
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


##### Distribution of TripType, Weekday, ScanCount, and DepartmentDescription
ggplot(train, aes(x = as.factor(TripType))) + geom_bar() + xlab("Trip Type")
ggplot(train, aes(x = as.factor(Weekday))) + geom_bar()
ggplot(train, aes(x = as.factor(ScanCount))) + geom_bar()
ggplot(train, aes(x = as.factor(DepartmentDescription))) + geom_bar()


##### TripTypes are 3~9, 12, 14, 15, 18~43, 999 where 999 is other

# Looking at type 40
type40 <- filter(train, TripType == 40)
type40 %>% 
  ggplot(aes(x = Weekday, fill = Weekday)) + geom_bar()
# Category of type 40
type40 %>%
  ggplot(aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
type40.nofood <- type40 %>%
  filter(DepartmentDescription != "GROCERY DRY GOODS") %>%
  filter(DepartmentDescription != "DSD GROCERY") %>%
  filter(DepartmentDescription != "PRODUCE") %>%
  filter(DepartmentDescription != "DAIRY")

ggplot(type40.nofood, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
max(table(type40$DepartmentDescription))


##### women-ish. Trying to isolate ladies
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

plus <- filter(train, DepartmentDescription == "PLUS AND MATERNITY")
ggplot(plus) + geom_bar(aes(x = TripType, fill = TripType)) + ggtitle("Plus and maternity trip type")
ggplot(filter(train, DepartmentDescription == "PERSONAL CARE")) + geom_bar(aes(x = TripType, fill = TripType))


type25.men <- filter(type25, DepartmentDescription == "MENS WEAR")
unique(filter(train, DepartmentDescription == "MENS WEAR")$Upc)  # 5563 Unique Upc
unique(type25.men$Upc) # 3518 Unique Upc


max(group_by(type25.men, Upc) %>% summarise(count = n()))

##### Looking at returns
returns <- filter(train, ScanCount < 0)
ggplot(returns, aes(x = TripType)) + geom_bar()
ggplot(returns, aes(x = Weekday, fill = Weekday)) + geom_bar() + ggtitle("Return Weekday Distribution")
ggplot(returns, aes(x = as.factor(ScanCount), fill = as.factor(ScanCount))) + geom_bar(show.legend = FALSE) + ggtitle("Return ScanCount")
ggplot(returns, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Return DepartmentDescription") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))
ggplot(returns, aes(x = FinelineNumber)) + geom_density()

returns.weekday <- filter(returns, Weekday %in% weekday)
returns.weekend <- filter(returns, Weekday %in% weekend)

# Weekday
ggplot(returns.weekday, aes(x = TripType)) + geom_bar()
ggplot(returns.weekday, aes(x = Weekday, fill = Weekday)) + geom_bar() + ggtitle("Return Weekday Distribution")
ggplot(returns.weekday, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Return DepartmentDescription") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

# Weekend
ggplot(returns.weekend, aes(x = TripType)) + geom_bar()
ggplot(returns.weekend, aes(x = Weekday, fill = Weekday)) + geom_bar() + ggtitle("Return Weekday Distribution")
ggplot(returns.weekend, aes(x = as.factor(ScanCount), fill = as.factor(ScanCount))) + geom_bar(show.legend = FALSE) + ggtitle("Return ScanCount")
ggplot(returns.weekend, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) + 
  ggtitle("Return DepartmentDescription") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

ggplot(returns, aes(x = TripType)) + geom_bar() + 
  facet_wrap(~Weekday) +
  ggtitle("Return Trip Type by day of Week") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(returns, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  facet_wrap(~Weekday) +
  geom_bar(show.legend = FALSE) + 
  ggtitle("Return Department by Day of Week") +
  theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 1))
ggsave("Department Facet.jpeg")

# Individual plots instead of facet
for(day in days) {
  temp <- filter(returns, Weekday == day)
  ggplot(temp, aes(x = TripType)) + geom_bar() + ggtitle(paste(day, "TripType.jpeg"))
  ggsave(paste(day, "TripType.jpeg"))
  ggplot(temp, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
    geom_bar(show.legend = FALSE) + 
    ggtitle(paste(day, "Return Department")) +
    theme(axis.text.x = element_text(angle = 55, hjust = 1))
  ggsave(paste(day, "Return Department.jpeg"))
}



df.dep.pos <- train %>% group_by(Weekday, DepartmentDescription)

# ScanCount/TotalScans, ReturnCount/TotalReturn, ReturnCount/TotalScans

df.dep <- train %>% 
  group_by(Weekday, DepartmentDescription) %>% 
  summarise(purchases = sum(ScanCount[ScanCount > 0]), returns = sum(ScanCount[ScanCount < 0]))

df.tot <- train %>% 
  group_by(DepartmentDescription) %>% 
  summarise(total_purchase = sum(ScanCount[ScanCount > 0]), total_return = sum(ScanCount[ScanCount < 0]))


df.dep <- left_join(df.dep, df.tot, by = c("DepartmentDescription" = "DepartmentDescription"))

df.dep$purchase_percent <- df.dep$purchases / df.dep$total_purchase * 100
df.dep$return_percent <- df.dep$returns / df.dep$total_return * 100
df.dep$return_rate <- abs(df.dep$returns) / df.dep$total_purchase * 100

department.name <- unique(df.dep$DepartmentDescription)
dep.1 <- df.dep %>% filter(DepartmentDescription %in% department.name[1:10])
dep.2 <- df.dep %>% filter(DepartmentDescription %in% department.name[11:20])
dep.3 <- df.dep %>% filter(DepartmentDescription %in% department.name[21:30])
dep.4 <- df.dep %>% filter(DepartmentDescription %in% department.name[31:40])
dep.5 <- df.dep %>% filter(DepartmentDescription %in% department.name[41:50])
dep.6 <- df.dep %>% filter(DepartmentDescription %in% department.name[51:60])
dep.7 <- df.dep %>% filter(DepartmentDescription %in% department.name[61:64])
dep.8 <- df.dep %>% filter(DepartmentDescription %in% department.name[65:69])

dep.9 <- df.dep %>% filter(DepartmentDescription %in% c("MENS WEAR", "LADIESWEAR"))

ggplot(dep.1) + 
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn1.jpeg", dpi = 600)
ggplot(dep.2) +   
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn2.jpeg")
ggplot(dep.3) + 
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn3.jpeg")
ggplot(dep.4) +   
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn4.jpeg")
ggplot(dep.5) + 
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn5.jpeg")
ggplot(dep.6) +   
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn6.jpeg")
ggplot(dep.7) + 
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn7.jpeg")
ggplot(dep.8) +   
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")
ggsave("SaleReturn8.jpeg")
ggplot(dep.9) +   
  geom_line(aes(x = Weekday, y = purchase_percent, group = 1), colour = "blue") +
  geom_line(aes(x = Weekday, y = return_percent, group = 1), colour = "red") +
  facet_wrap(~ DepartmentDescription, ncol = 3) + ggtitle("Purchase Percent, Return Percent")

# df.dep.neg <- train %>% filter(ScanCount < 0) %>% group_by(Weekday, DepartmentDescription) %>% summarise(returns = sum(ScanCount), n = n())
# df.tot.neg <- train %>% filter(ScanCount < 0) %>% group_by(Weekday) %>% summarise(total_return = sum(ScanCount), total_return_occurance = n())
# df.dep.neg <- left_join(df.dep.neg, df.tot.neg, by = c("Weekday" = "Weekday"))
# df.dep.neg$return_percent <- df.dep.neg$returns / df.dep.neg$total_return * 100





# Looking at mass buys
buy_count <- train %>% 
  filter(ScanCount > 0) %>%
  group_by(VisitNumber, TripType) %>%
  summarise(count = sum(ScanCount))


ggplot(buy_count) + geom_histogram(aes(x = count))
ggplot(filter(buy_count, count > 25)) + geom_bar(aes(x = TripType, fill = TripType))


type41 <- filter(train, TripType == 41)
ggplot(type41, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
  geom_bar(show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



types <- c(c(3:9), c(12,14,15), c(18:43), 999)
for (i in types) {
  temp <- filter(train, TripType == i)
  ggplot(temp, aes(x = DepartmentDescription, fill = DepartmentDescription)) + 
    geom_bar(show.legend = FALSE) + ggtitle(paste(i, "distribution")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste(i,"distribution.jpeg"))
}

impulse <- filter(train, DepartmentDescription == "IMPULSE MERCHANDISE")
ggplot(impulse) + geom_bar(aes(x = TripType, fill = TripType))

ggplot(filter(train, DepartmentDescription == "GROCERY DRY GOODS")) + geom_bar(aes(x = TripType, fill = TripType))
ggplot(filter(train, DepartmentDescription == "DSD GROCERY")) + geom_bar(aes(x = TripType, fill = TripType))


visit_transaction <- train %>%
  group_by(VisitNumber, DepartmentDescription, TripType) %>%
  summarise(dep_transaction = sum(abs(ScanCount)))

total_transaction <- train %>%
  group_by(VisitNumber) %>%
  summarise(total_transaction = sum(abs(ScanCount)))

visit_transaction <- left_join(visit_transaction, total_transaction, by = c("VisitNumber" = "VisitNumber"))
visit_transaction$fraction <- visit_transaction$dep_transaction / visit_transaction$total_transaction

temp <- visit_transaction %>%
  filter(DepartmentDescription == "FINANCIAL SERVICES") %>%
  filter(fraction > 0.7)

temp2 <- filter(visit_transaction, TripType == 3, DepartmentDescription != "FINANCIAL SERVICES")
View(filter(visit_transaction, DepartmentDescription == "IMPULSE MERCHANDISE", fraction == 1))
