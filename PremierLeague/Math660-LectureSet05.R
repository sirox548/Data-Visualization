
library(tidyverse)

table1 <- tibble(country=c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
                 year = c(1999L,2000L,1999L,2000L,1999L,2000L),
                 cases=c(745L,2666L,37737L,80488L,212258L,213766L),
                 population=c(19987071L,20595360L,172006362L,174504898L,1272915272L,1280428583L)
)

table2 <- tibble(country=c(rep("Afghanistan", 4), rep("Brazil", 4), rep("China",4)),
                 year = rep(rep(c(1999L,2000L), each=2),3),
                 type=rep(c("cases","population"),6),
                 count=c(745L,19987071L,2666L,20595360L,37737L,172006362L,80488L,
                         174504898L,212258L,1272915272L,213766L,1280428583L))

table3 <- tibble(country=c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
                 year=rep(c(1999L,2000L), 3),
                 rate=c("745/19987071", "2666/20595360", "37737/172006362",
                        "80488/174504898", "212258/1272915272", "213766/1280428583"))

table4a <- tibble(country=c("Afghanistan", "Brazil", "China"),
                  `1999`=c(745L,37737L,212258L),
                  `2000`=c(2666L,80488L,213766L))

table4b <- tibble(country=c("Afghanistan", "Brazil", "China"),
                  `1999`=c(19987071L, 172006362L, 1272915272L),
                  `2000`=c(20595360L, 174504898L, 1280428583L))

aable1 %>% mutate(rate = cases/population * 10000)

table1 %>% count(year, wt=cases)


ggplot(table1, aes(year, cases)) + geom_line(aes(group=country), colour="grey50") +
  geom_point(aes(colour=country))


#### pivot_longer
table4a
tidy4a <- table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to="year", values_to="cases")
tidy4a

table4b
tidy4b <- table4b %>%
  pivot_longer(c(`1999`, `2000`), names_to="year", values_to="population")
tidy4b

left_join(tidy4a, tidy4b)

### pivot_wider
table2
table2 %>% pivot_wider(names_from=type, values_from=count)

### separating values
table3
table3 %>% separate(rate, into=c("cases", "population"))
### use better types for new variables
table3 %>% separate(rate, into=c("cases", "population"), convert=TRUE)

### unite
table5 <- table3 %>% separate(rate, into=c("cases", "population"), convert=TRUE)
table5 %>% unite(rate, cases, population, sep="/")

###### missing values
stocks <- tibble(year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
                 qtr = c(1,2,3,4,2,3,4),
                 return=c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66))
### make missing values explicit
stocks %>% pivot_wider(names_from=qtr, values_from=return)
stocks %>% complete(year, qtr)

### dropping missing values
stocks1 <- stocks %>% pivot_wider(names_from=qtr, values_from=return)
stocks1
stocks1 %>% pivot_longer(cols=c(2:5), names_to="qtr", values_to="return", values_drop_na=TRUE)

#########
### Relational data
library(nycflights13)

flights

airlines

airports

planes

weather

### tailnum is a primary key for planes
planes %>% count(tailnum) %>% filter(n>1)

### not primary key
weather %>% count(year, month, day, hour, origin) %>% filter(n>1)

### sometimes there is no primary key
flights %>% count(year, month, day, hour, tailnum) %>% filter(n>1)

## a mutating join
flights %>% select(year, month, day, hour, tailnum, carrier) %>%
  left_join(airlines, by="carrier")


### a filtering join: semi_join and anti_join
flights1 <- flights %>% select(year, month, day, hour, carrier, flight, origin, dest, tailnum)

topflights <- flights1 %>% count(origin, dest, sort=TRUE) %>% head(3)
topflights
### find flights that match these top origin-destination pairs

flights1 %>% semi_join(topflights)

semi_join(flights1, topflights) ### does the same thing

### anti-join
flights %>% anti_join(planes, by="tailnum")  ### flights that do not have a match in planes

flights %>% anti_join(planes, by="tailnum") %>% count(tailnum, sort=TRUE)


### left join using all variables
flights1 %>% left_join(weather)

flights1 %>% left_join(planes, by="tailnum")

flights1 %>% left_join(weather, by=c("origin", "year", "month", "day", "hour"))

flights1 %>% left_join(airports, by=c("dest"="faa"))



