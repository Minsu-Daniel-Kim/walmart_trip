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



df.dep <- train %>% filter(ScanCount > 0) %>% group_by(Weekday, DepartmentDescription) %>% summarise(sum = sum(ScanCount), n = n())
df.tot <- train %>% group_by(Weekday) %>% summarise(sum_tot = sum(ScanCount), n_tot = n())
df.dep <- left_join(df.dep, df.tot, by = c("Weekday" = "Weekday"))
df.dep$percent_sum <- df.dep$sum / df.dep$sum_tot * 100
df.dep$percent_n <- df.dep$n / df.dep$n_tot * 100





# Looking at mass buys




# Plus and Maternity
print("hi")

