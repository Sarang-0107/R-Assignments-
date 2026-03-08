#Basic Summary (Average, Sum, Count)
df <- data.frame(
  sales = c(100, 200, 150, 300)
)
df

library(dplyr)

df |>  summarise(total_sales=sum(sales),
                 avg_sales=mean(sales),
                 max_sales=max(sales),
                 count=n()
)
#Summarise by Group

df <- data.frame(
  region = c("Asia","Asia","Europe","Europe","North Carolina", "North America","South africa"),
  sales = c(100,200,150,300,250,400,350)
)

df |> 
  group_by(region) |> 
  summarise((total_sales = sum(sales)))

install.packages(tidyverse)
library(tidyverse)
library(nycflights13)


flights |>
  arrange(carrier) |>
  group_by(carrier) |>
  summarise(first_year = nth(year,10))

flights |> group_by(carrier) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),  month =
  nth(month, 7), dep_time = nth(day, 2), arr_time = nth(day, 2)) |>
  arrange(desc(avg_delay))

View(flights)

#Multiple Summaries Per Group

flights |> 
  group_by(carrier) |> 
  summarise(
    avg_delay = mean(arr_delay), na.rm = TRUE,
    total_dealy = sum(arr_delay),
    max_delay = max(arr_delay)
  )

#Counting Rows
df |> 
  summarise(count = n())

flights |> 
  group_by(carrier) |> 
  summarise(count = n())

#Percentage Calculation
df |> 
  summarise(
    total_sales = sum(sales),
    avg_sales = mean(sales)
  )

  #or

df |> 
  group_by(region) |> 
  summarise(total_sales = sum(sales)) |> 
  mutate(percent = total_sales / sum(total_sales))
