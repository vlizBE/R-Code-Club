####DPLYR####

library(dplyr)

??dplyr   #"Introduction to dplyr"

####DATA####------------------------------------------------------------------------------------

#example build-in dataset
library(nycflights13)

# get the dimentions of the dataset  (rows - columns)
dim(flights)

#view data: !tibble (dimensions displayed too)
flights

data <- flights

#get a glimpse of the data: view all columns and some of the values
glimpse (flights)


####  SIMPLE VERBS  (FUNCTIONS)  ####------------------------------------------------------------------------------------
#first argument is the data frame

#distinct for getting all unique variables in a column 
distinct(flights,tailnum) #number of planes


#filter for certain rows with specific columns
filter(flights, month==4, day==12)

flights_april_12 <- filter(flights, month==4,day==12)  #create a new tibble


#arrange for reordering columns
arrange(flights_april_12, sched_dep_time)

#all flights from the 12th of april, in descending order
flights%>%         
  filter(month==4,day==12) %>%
  arrange(desc(sched_dep_time))
# ---->   use of pipeline: this executes all functions on 1 dataset, you only mention the dataset once.


#select for selecting columns
select(flights, year, month,day)
select(flights,year:day)   #select all columns in between
select(flights, -(year:day))  #select all, except for those columns
select(flights, ends_with("time"))


#mutate for creating new columns and adding them to the dataset
Result <-mutate(flights,
    gain = arr_delay - dep_delay,
    speed= distance/air_time*60)

#transmute for only keeping the created columns, drop all other columns (different result)
transmute(flights,
  gain = arr_delay - dep_delay,
  speed= distance/air_time*60)

#summarise for summarising columns   (very usefull in combination with group_by())
summarise(flights, delay=mean(dep_delay, na.rm=TRUE))       #na.rm=TRUE ReMoves the na values when calculating


#getting all unique planes (tailnum) and some stats
stats <- flights%>%
  group_by(tailnum)%>%
  summarise(count=n(),
            dist=mean(distance, na.rm=TRUE), 
            delay=mean(arr_delay, na.rm = TRUE)) %>%
  arrange(dist)

flights_per_month <- flights%>%
  group_by(month)%>%
  count()%>%
  rename(tot_flights=n)%>%
  arrange(desc(tot_flights))

average_delay_per_month <- flights%>%
  group_by(month)%>%
  summarize(average_delay=mean(arr_delay, na.rm = TRUE))%>%
  arrange(desc(average_delay))





#make a tibble from a data frame
df <- data.frame(
  x <- c(1,2,3),
  y <- c(8,7,0)
)

df2 <- tbl_df(df)

