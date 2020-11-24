
library(tidyverse)

#### Strings    

strsplit("www.yahoo.com", split="[.]")
strsplit("www.yahoo.com", split="h")

strsplit("www.yahoo.com", split=".")


paste("Probability", "and", "Statistics")
paste("Probability", "and", "Statistics", sep="-")


library(stringr)

str_c("a", c("b", "c", "d"), "e") 
str_c("a", c("b", "c", "d"), "e", collapse="")


str_length("Math660")

str_sub("Mathematics", 1,4) 
str_sub("Mathematics", -4,-1) 

str_to_lower("HI!")
str_to_upper("bye")
str_to_title("sTaTiStIcS")

str_split("www.yahoo.com", pattern="[.]")

str_split(c("The cat is grey", "I love cats"), pattern=" ")


str_split(c("The cat is grey", "I love cats"), pattern=" ", simplify=TRUE) 
str_split("I like cats. Do you like cats", " ")[[1]]


str_split("I like cats. Do you like cats?", boundary("word"))
str_split("I like cats. Do you like cats?", boundary("word"))[[1]]

str_sort(c("cat", "bat", "cow"))
str_order(c("cat", "bat", "cow"))


##### Factors

x1 <- c("Dec", "Apr", "Jan", "Mar")

sort(x1)

month.levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

month <- factor(x1, levels=month.levels)
sort(month)

month <- factor(x1)
sort(month)

x2 <- c("Dec", "Apr", "Jam", "Mar")
month <- factor(x2, levels=month.levels)
month

### Dates and Times

library(lubridate)

ymd("2019-03-31") 
mdy("March 31st, 2019")
dmy("31-Jan-2019")
ymd(20190131)

ymd_hms("2019-03-31 22:15:35")
mdy_hm("Jan 31 2019 10:15")


ymd(20190131, tz="UTC")
ymd(20190131, tz="GMT")
ymd(20190131, tz="Asia/Singapore")

ymd(20190131, tz="America/New_York")



library(tidyverse) 
library(lubridate) 
library(nycflights13)
library(ggplot2)

flights %>% select(year, month, day, hour, minute) 

flights1 <- flights %>% select(year, month, day, hour, minute) 

flights1 %>% mutate(departure = make_datetime(year, month, day, hour, minute))



flights1 %>% mutate(departure = make_datetime(year, month, day, hour, minute)) %>% ggplot(aes(departure)) + geom_freqpoly(binwidth=86400) # 86400 seconds = 1 day

## frequency plot of counts of departing flights by day
flights1 %>% mutate(departure = make_datetime(year, month, day, hour, minute)) %>% ggplot(aes(departure)) +
  geom_freqpoly(binwidth=86400) # 86400 seconds = 1 day

### frequency plot of counts of departing flights within a day (20130102) in 10 minute intervals
flights1 %>% mutate(departure = make_datetime(year, month, day, hour, minute)) %>% filter(departure < ymd(20130102)) %>% ggplot(aes(departure)) + geom_freqpoly(binwidth=600) # 600 seconds = 10 minute
  
flights1 %>% mutate(departure = make_datetime(year, month, day, hour, minute)) %>% 
  filter(departure < ymd(20130102)) %>% ggplot(aes(departure)) +
  geom_freqpoly(binwidth=600) # 600 seconds = 10 minute

as_datetime(today())
as_date(now())
  
## Unix Epoch is set as Jan 1, 1970

as_date(390000)
as_datetime(390000)
  
datetime <- ymd_hms("2016-07-08 12:34:56")
month(datetime)
mday(datetime) 
yday(datetime)


wday(datetime)
wday(datetime, label=TRUE)
wday(datetime, label=TRUE, abbr=FALSE)


3 * dyears(2)
dyears(2) + dweeks(3) + ddays(1)

tomorrow <- today() + ddays(1)
last.year <- today() - dyears(1)


one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York") 
one_pm 
one_pm + ddays(1) 



one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York") 
one_pm 
one_pm + days(1) 



dyears(1)/ddays(1)
years(1)/days(1) 


 next.year1 <- dmy("1 Jan 2020") + years(1)
 (dmy("1 Jan 2020") %--% next.year1) / days(1) 
 next.year2 <- dmy("1 Jul 2020") + years(1) 
 (dmy("1 Jul 2020") %--% next.year2) / days(1) 




library(forcats)
gss_cat

gss_cat %>% count(race)

ggplot(gss_cat, aes(race)) + geom_bar()

religion.summary <- gss_cat %>% 
    group_by(relig) %>% 
    summarise(age = mean(age, na.rm=TRUE), 
              tvhours = mean(tvhours, na.rm=TRUE), 
              n = n())


ggplot(religion.summary, aes(tvhours, relig)) +   geom_point()


religion.summary %>% 
    mutate(relig=fct_reorder(relig, tvhours)) %>% 
    ggplot(aes(tvhours, relig)) + geom_point()


### reordering the levels
levels(gss_cat$rincome)
levels(fct_relevel(gss_cat$rincome, "Not applicable"))


   marital.by.age <- gss_cat %>% 
     filter(!is.na(age)) %>% 
     count(age, marital) %>% 
     group_by(age) %>% 
     mutate(prop = n / sum(n))

   ggplot(marital.by.age, aes(age, prop,   colour=marital)) + geom_line(na.rm=TRUE)

##  fct_reorder2(fct, x, y)
#does something quite complicated: it reorders the levels of fct the values of y associated with the largest x.

 ggplot(marital.by.age, aes(age, prop,   colour = fct_reorder2(marital, age, prop))) +
   geom_line() +  labs(colour = "marital")   #### legends are arranged to match the lines

### ordering by frequency
  gss_cat %>%   mutate(marital = marital %>% fct_infreq() %>% 
     fct_rev()) %>% 
     ggplot(aes(marital)) + geom_bar()

#### renaming levels
#  fct_recode(fct, newlevel1 = oldlevel1, ...)

### collapsing levels
#    fct_recode(fct, newlevel1 = oldlevel1, newlevel1 = oldlevel2, ...)

levels(gss_cat$partyid)

    gss_cat %>% mutate(partyid = fct_collapse(partyid, 
       Rep = c("Strong republican", "Not str republican"), 
       Dem = c("Strong democrat", "Not str democrat"), 
       Ind = c("Ind,near rep", "Independent", "Ind,near dem"), 
       Other = c("No answer", "Don't know", "Other party") 
      )) %>% count(partyid)


### lump smallest classes together
  gss_cat %>% 
     mutate(relig = fct_lump(relig, n=10)) %>% 
     count(relig, sort=TRUE)



