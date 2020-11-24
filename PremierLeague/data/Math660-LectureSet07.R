
library(tidyverse)


#### Facebook data again

facebook <- read_tsv("pseudo_facebook.txt")

ggplot(data=facebook, aes(x=age, y=friend_count)) + geom_point()

ggplot(data=facebook, aes(x=age, y=friend_count)) + geom_point() + xlim(13,90)

ggplot(data=facebook, aes(x=age, y=friend_count)) + geom_jitter(alpha=1/20) + xlim(13,90)

ggplot(data=facebook, aes(x=age, y=friendships_initiated+0.01)) + geom_point(alpha=1/20, position=position_jitter(h=0)) +
  coord_trans(y='log')

ggplot(data=facebook, aes(x=age, y=friendships_initiated)) + geom_point(alpha=1/20, position=position_jitter(h=0)) +
  coord_trans(y='sqrt')


### more on facebook data

age.groups <- group_by(facebook, age)

facebook.summary <- summarize(age.groups, mean.friend.count=mean(friend_count), median.friend.count=median(friend_count), n=n())

ggplot(data=facebook.summary, aes(x=age, y=mean.friend.count)) + geom_point()

ggplot(data=facebook.summary, aes(x=age, y=mean.friend.count)) + geom_line()

ggplot(data=facebook, aes(x=age, y=friend_count)) + 
  geom_point(alpha=1/20, position=position_jitter(h=0), color="orange") +
  coord_trans(y='sqrt')

ggplot(data=facebook, aes(x=age, y=friend_count)) + 
  geom_point(alpha=1/20, position=position_jitter(h=0), color="orange") +
  coord_trans(y='sqrt') + xlim(13,90) +
  geom_line(stat='summary', fun.y=mean)

ggplot(data=facebook, aes(x=age, y=friend_count)) + 
  geom_point(alpha=1/20, position=position_jitter(h=0), color="orange") +
  coord_trans(y='sqrt') + xlim(13,90) +
  geom_line(stat='summary', fun.y=mean) +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.1), linetype=2, color='blue') +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.9), linetype=2, color='blue') +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.5),  color='blue')

ggplot(data=facebook, aes(x=age, y=friend_count)) + 
  geom_point(alpha=1/20, position=position_jitter(h=0), color="orange") +
  coord_cartesian(xlim=c(13,70), ylim=c(0,1000)) +
  geom_line(stat='summary', fun.y=mean) +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.1), linetype=2, color='blue') +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.9), linetype=2, color='blue') +
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.5),  color='blue')

### correlation

cor(facebook$age, facebook$friend_count)
cor.test(facebook$age, facebook$friend_count)

with(facebook, cor.test(age, friend_count))
with(subset(facebook,age<=70), cor.test(age, friend_count))

### understanding noise
p1 <- ggplot(data=filter(facebook.summary, age<71), aes(x=age, y=mean.friend.count)) + geom_line(size=0.8)
p1

age.groups.month <- group_by(facebook, dob_year, dob_month)
facebook.summary.month <- summarize(age.groups.month, age=mean(age), mean.friend.count=mean(friend_count), median.friend.count=median(friend_count), n=n())


p2 <- ggplot(data=filter(facebook.summary.month, age<71), aes(x=age, y=mean.friend.count)) + geom_line()
p2


library(gridExtra)
grid.arrange(p1, p2, ncol=1)

p1 + geom_line(data=filter(facebook.summary.month, age<71), aes(x=age, y=mean.friend.count), linetype=3, colour="blue")

############### dplyr

library(nycflights13)
library(tidyverse)


### filter() - select columns
filter(flights, month==1, day==1)
flights.jan1 <- filter(flights, month==1, day==1)

filter(flights, month==1 | month == 2)
filter(flights, month %in% c(1,2,3))
filter(flights, arr_delay > 120 | dep_delay > 120)
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120,  dep_delay <= 120)

x <- NA
is.na(x)

filter(flights, is.na(dep_delay))


### arrange() - reorder observations
arrange(flights, year, month, day)
arrange(flights, desc(arr_time))
  

### using base R
flights[(flights$month==1 & flights$day==1), ]

oo<-order(flights$year, flights$month, flights$day) 
flights[oo, ]

###  select() allows us to extract a subset of the variables in a dataset:
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))


select(flights, time_hour, air_time, everything())
rename(flights, destination=dest, airline=carrier)

flights1 <- rename(flights, destination=dest, airline=carrier)

flights1 <- select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights1, speed = distance * 60 / air_time, gain = dep_delay - arr_delay)
num_range("x", 1:3) matches x1, x2 and x3.

mutate(flights1, gain = dep_delay-arr_delay, 
       hours = air_time/60, 
       gain_per_hour = gain/hours)

transmute(flights1, gain = dep_delay-arr_delay, 
          hours = air_time/60, 
          gain_per_hour = gain/hours)


summarise(flights, mean.delay = mean(dep_delay, na.rm=TRUE))
by_day = group_by(flights, year, month, day) 
summarise(by_day, delay=mean(dep_time, na.rm=TRUE))

### Using pipes
#### diamonds dataset example
diamonds <- read.csv("diamonds.csv", header=T)

diamonds1 <- filter(diamonds, color == "I")
diamonds2 <- group_by(diamonds1, cut)
diamonds3 <- summarise(diamonds2,   mean.price = mean(price))

summarise(group_by( 
    filter(diamonds, color=="I"), cut), 
    mean.price = mean(price))
    
diamonds %>% 
    filter(color=="I") %>% 
    group_by(cut) %>% 
    summarise(mean.price=mean(price))

### nycflights13 example
flights.by.dest <- group_by(flights, dest)
dest.delay <- summarise(flights.by.dest, 
                        count = n(), 
                        dist = mean(distance, na.rm=TRUE), 
                        delay = mean(arr_delay, na.rm=TRUE)) 
dest.delay <- filter(dest.delay, count > 20, 
                     dest != "HNL")

ggplot(dest.delay, aes(x=dist, y=delay)) + 
    geom_point(aes(size=count), alpha=1/3) + 
    geom_smooth(se=FALSE)

### doing it with pipes
flights %>% 
    group_by(dest) %>%
    summarise(count = n(), 
              dist = mean(distance, na.rm=TRUE), 
              delay = mean(arr_delay, na.rm=TRUE)) %>% 
    filter(count > 20, dest != "HNL")

### can even pipe to ggplot
flights %>% 
    group_by(dest) %>%
    summarise(count = n(), 
              dist = mean(distance, na.rm=TRUE), 
              delay = mean(arr_delay, na.rm=TRUE)) %>% 
    filter(count > 20, dest != "HNL") %>%
    ggplot(aes(x=dist, y=delay)) + 
    geom_point(aes(size=count), alpha=1/3) + 
    geom_smooth(se=FALSE)  ### get same plot


### grouping and ungrouping; different results from summarise
daily.flights <- group_by(flights, year, month, day)

daily.flights %>% summarise(flights=n())
daily.flights %>% 
    ungroup() %>% 
    summarise(flights=n())

