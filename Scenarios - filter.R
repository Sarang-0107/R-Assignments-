#🧪 1. Column Selection Scenarios
#Select columns that are numeric and contain at least one missing value
#Select columns where all values are positive
#Select columns where more than 50% of values are > 100
#Select columns that have high variance
#Select columns whose names start with “dep” or “arr”

#Q1.#Select columns that are numeric and contain at least one missing value

install.packages("nycflights13")
library(dplyr)
library(nycflights13)

flights %>% 
  select(where(~is.numeric(.) && any(is.na(.)) ))

flights %>% 
  select(where(~ is.numeric(.) && any(is.na(.))))

#Select columns where all values are positive
flights %>% 
  select(where(~is.numeric(.) && all(. > 0)))

flights %>% 
  select(where(~ is.numeric(.) && all(. > 0)))

flights %>% select(where(~ is.numeric(.) && all(. > 0, na.rm = TRUE)))

##Select columns where more than 50% of values are > 100
flights %>% 
  select(where( is.numeric ,~.x>100))

flights |> 
  filter(if_all(where(is.numeric),~.x>100))

  flights %>% 
    select(where(~ is.numeric(.) && mean(. > 100, na.rm =TRUE) > 0.5))
  
  flights %>% 
    select(where(function(x) is.numeric(x) && mean(x > 100, na.rm = TRUE) > 0.5))
  #all() → “everything passes?”
  #any() → “at least one passes?”
  #mean() → “fraction that passes?” → use for proportions like >50%
  

##Select columns that have high variance

  flights %>% 
    select(where(~ is.numeric(.) && var(., na.rm = TRUE) >1e6))

##Select columns whose names start with “dep” or “arr”
  flights %>% 
    select(starts_with("dep"), starts_with("arr"))

  
#🔍 2. Row Filtering Scenarios
# Filter rows where any numeric column > 100
#Filter rows where all numeric columns are within a valid range
#Filter rows where exactly 2 numeric columns exceed a threshold
#Filter rows where at least 3 columns are missing
#Filter rows where no numeric column is negative  
  
#Q1:- # Filter rows where any numeric column > 100
flights %>% 
  filter(if_any(where(is.numeric), ~ . > 100))

#Q2.#Filter rows where all numeric columns are within a valid range

flights %>%
  filter( 
  if_all(where(is.numeric), ~ .x >= 0 ))

flights %>%
  filter(
    if_all(c(dep_delay,arr_delay,air_time,distance),~ !is.numeric(.x) & .x >= 0
           )
  )

#Q3.#Filter rows where exactly 2 numeric columns exceed a threshold
#Q4.#Filter rows where at least 3 columns are missing

flights %>% 
  filter(rowSums(is.na(.)) >=3 )


#Q5.Filter rows where no numeric column is negative 

flights %>% 
  filter(if_all(where(is.numeric), ~ .x <= 0))


#✈️ 3. Flights Dataset Scenarios
#Find flights that departed earliest each day
#Find flights that arrived latest for each destination
#Get carriers that fly to more than 10 unique destinations
#Find the most frequent route (origin → dest)
#Identify flights with maximum delay per carrier
#Find days where all flights were delayed
#Get flights where departure delay > arrival delay

#Q1.#Find flights that departed earliest each day
flights %>% 
  #group_by(year,month,day) %>% 
  filter((dep_time ==  min(dep_time, na.rm = TRUE))) %>% 
  ungroup()

#Q2.Find flights that arrived latest for each destination
flights %>%
  group_by(year,month,day) %>%
  filter((arr_time == max(arr_time,na.rm =TRUE)))%>% 
  ungroup()

#Q3.Get carriers that fly to more than 10 unique destinations
flights %>% 
  group_by(carrier) %>%  
  summarise(n_dest = n_distinct(dest)) %>% 
  filter(n_dest > 10) 

flights %>% 
  group_by(carrier) %>% 
  filter(n_dest =n_distinct(dest) > 10 ) %>% 
  ungroup() %>%  select( carrier,dest)



flights %>%
  group_by(carrier) %>% mutate(n_dest= n_distinct(dest)) %>% 
  filter(n_dest > 10) %>%  # just the condition
  ungroup() %>%
  select(carrier, dest, n_dest)

#Q4.Find the most frequent route (origin → dest)
flights %>% 
  group_by(origin,dest) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count))

#Q5.Identify flights with maximum delay per carrier
flights %>% 
  group_by(carrier) %>% 
  filter(dep_delay == max(dep_delay, na.rm =TRUE)) %>% 
  ungroup()


#Q6.Find days where all flights were delayed
flights %>% group_by(year,month,day) %>% 
  filter(all(dep_delay > 0, na.rm =TRUE)) %>% 
  ungroup()

#Or

flights %>%
  group_by(year, month, day) %>%
  filter(all(dep_delay > 0, na.rm = TRUE)) %>%  # all flights delayed
  ungroup()

#Q7.#Get flights where departure delay > arrival delay
flights %>% filter(dep_delay > arr_delay) %>% 
  mutate(diff= dep_delay - arr_delay) %>% 
  select(diff,dep_delay,arr_delay)

#4. Duplicate Handling Scenarios
#Identify fully duplicate rows
#Find duplicates based on subset of columns
#Keep only the first occurrence of each duplicate group
#Keep only the last occurrence based on time
#Count how many times each flight appears per day
#Flag rows as duplicate vs unique
#Remove duplicates but preserve the most recent record

#Q1.#Identify fully duplicate rows.

a <- flights %>%
  mutate(dup = duplicated(.)) 
a
View(a)

#Q2.#Find duplicates based on subset of columns.

flights %>% 
  filter(duplicated(select(.,carrier,flight)))

#Q.3 Keep only the first occurrence of each duplicate group
flights %>%  
  filter( !duplicated(select(.,carrier,dep_time,dep_delay,flight,origin)))

library(dplyr)

df <- tibble(
  carrier   = c("AA", "AA", "AA", "DL", "DL"),
  dep_time  = c(500, 500, 600, 700, 700),
  dep_delay = c(10, 10, 5, 0, 0),
  flight    = c(1, 1, 2, 3, 3),
  origin    = c("JFK", "JFK", "JFK", "LGA", "LGA")
)

df

df %>%
  filter(!duplicated(select(., carrier, dep_time, dep_delay, flight, origin)))

#Q4.Keep only the last occurrence based on time

flights %>% 
  arrange(desc(carrier),desc(dep_time),desc(dep_delay)) %>% 
  filter(!duplicated(select(., carrier, dep_time, dep_delay)))

#Q5.Count how many times each flight appears per day
flights %>% 
  count(year,month,day)

#Q6.Flag rows as duplicate vs unique

library(dplyr)

 af <-flights %>%
  group_by(carrier, dep_time, dep_delay) %>%
  mutate(flag = ifelse(n() > 1, "Duplicate", "Unique")) %>%
  ungroup()
af
View(af)

#Q7.Remove duplicates but preserve the most recent record
rup <- flights_tbl %>% 
  arrange(desc(carrier),desc(dep_time),desc(dep_delay),desc(arr_time)) %>% 
  distinct(carrier,dep_time,dep_delay,arr_time, .keep_all = TRUE) %>% 
  select(carrier,dep_time,dep_delay,arr_time) %>%  head(20)
rup
View(rup)

library(tibble)
library(readr)

library(tibble)
library(dplyr)
library(lubridate)

set.seed(123)

flights_tbl <- tibble(
  year       = rep(2013, 60),
  month      = rep(1:6, each = 10),
  day        = rep(1:10, times = 6),
  dep_time   = sample(seq(500, 2300, by = 5), 60),
  dep_delay  = sample(-10:40, 60, replace = TRUE),
  arr_time   = sample(seq(600, 2359, by = 5), 60),
  arr_delay  = sample(-20:50, 60, replace = TRUE),
  carrier    = sample(c("AA", "DL", "UA", "B6", "US", "WN"), 60, replace = TRUE),
  flight     = sample(1:3000, 60),
  origin     = sample(c("JFK", "LGA", "EWR"), 60, replace = TRUE),
  dest       = sample(
    c("ATL","ORD","LAX","DFW","DEN","SFO","CLT","LAS","MIA","SEA"),
    60, replace = TRUE
  ),
  air_time   = sample(40:360, 60, replace = TRUE),
  distance   = sample(200:2600, 60, replace = TRUE)
) 
flights_tbl
View(flights_tbl)

#5. Grouping & Aggregation Scenarios
#Calculate average delay per carrier
#Count number of flights per destination
#Find minimum and maximum delay per day
#Get carriers with average delay > threshold
#Find destinations with highest traffic
#Count number of flights per hour
#Get summary stats (mean, median, sd) per group

#Q1.Calculate average delay per carrier

g <- flights %>% 
  group_by(carrier) %>% 
  mutate(avg_mean=mean(arr_delay, na.rm = TRUE)) %>% 
  select(carrier,avg_mean,arr_time)
g
View(g)

h <- flights %>% 
  group_by(carrier) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) 
h
View(h)

#Find carrier with highest average delay
j <- flights %>% 
  group_by(carrier) %>% 
  summarise(hig_delay=mean(arr_delay,na.rm = TRUE)) %>% 
  arrange(desc(hig_delay))
j

#delay difference from daily average
flights %>% 
  group_by(year,month,day) %>% 
  mutate(
    daily_avg=mean(arr_delay ,na.rm = TRUE),
    diff=arr_delay - daily_avg )%>% 
  select(daily_avg,carrier,arr_delay,dep_delay,year,month,day)

#“highest average” → summarise() + arrange()
#“difference from group average” → mutate()

#Q2.Count number of flights per destination
flights %>% 
  group_by(dest) %>% 
  summarise(n_flights = n()) 

#Q3.Find minimum and maximum delay per day
flights %>% 
  group_by(year,month,day) %>% 
  summarise(
    min_delay=min(arr_delay, na.rm = TRUE),
    max_dealy =max(arr_delay, na.rm = TRUE)
  )

#Q4.Get carriers with average delay > threshold

threshold <- 10
flights %>% 
  group_by(carrier) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm =TRUE)) %>% 
  filter(avg_delay > threshold)

#Q5.Find destinations with highest traffic
flights %>% 
  group_by(dest) %>% 
  summarise(high_traffic=n()) %>% 
  arrange(desc(high_traffic))
  
#Q6.Count number of flights per hour
flights %>% 
  group_by(hour) %>% 
  summarise(n_hour = n())

#Q7.Get summary stats (mean, median, sd) per group
flights %>% 
  group_by(dest) %>% 
  summarise(mean_delay= mean(dep_time,na.rm=TRUE),
            meadian_delay = median(dep_time, na.rm = TRUE),
            sd_delay = sd(dep_time, na.rm=TRUE)
  )


#6. Mutation / Derivation Scenarios
#Create a delay category (on-time, minor, major)
#Create a binary flag for delay > 15 minutes
#Calculate total delay (departure + arrival)
#Derive speed = distance / air_time
#Create a column showing difference between two variables
#Standardize numeric columns (z-score)
#Create categories based on quantiles

#Q1.Create a delay category (on-time, minor, major)

flights %>% 
  mutate(
    delay_category = case_when(
      arr_delay <= 0 ~ "On-time",
      arr_delay >0 & arr_delay < 30 ~ "Minor",
      arr_delay > 30 ~ "Major",
      TRUE ~ NA_character_
    ) 
  ) %>% 
  select(carrier, arr_delay,arr_time,delay_category)


# Q2.Create a binary flag for delay > 15 minutes

flights %>% 
  mutate(binary_flag= ifelse( arr_delay > 15 ,1,0)) %>% 
  select(carrier, arr_delay,arr_time,binary_flag)

#Q3.#Calculate total delay (departure + arrival)
flights %>%  
  mutate(total_delay = dep_delay + arr_delay) %>% 
  select(carrier, arr_delay,arr_time,total_delay)

#Q4.#Derive speed = distance / air_time

flights%>% 
  mutate(speed=distance/air_time) %>% 
  select(carrier, arr_delay,arr_time,speed,air_time,distance)


#7. Logical / Conditional Scenarios
#Flag rows where any condition is TRUE across columns
#Flag rows where all conditions are TRUE
#Identify rows where values are inconsistent
#Create flags for outliers
#Check if multiple conditions occur together
#Flag rows where value crosses threshold compared to previous row

#Q1.#Flag rows where any condition is TRUE across columns

flights %>% 
  mutate(flag_any= if_any(c(sched_dep_time,arr_time,arr_delay,dep_time,dep_delay,sched_arr_time), ~. > 100)) %>% 
  select(sched_dep_time,arr_time,arr_delay,dep_time,dep_delay,sched_arr_time,flag_any)

#Q2.#Flag rows where all conditions are TRUE
flights %>% 
  mutate(all = if_all(sched_dep_time,arr_time,arr_delay,dep_time,dep_delay,sched_arr_time), ~ x > 10) %>% 
  select(sched_dep_time,arr_time,arr_delay,dep_time,dep_delay,sched_arr_time,all)
