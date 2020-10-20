# 
# 
# USDA database selector https://quickstats.nass.usda.gov
# 
# berries
# https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1
# 
# 
# corn farming economics
# https://quickstats.nass.usda.gov/results/471133FF-98EA-306C-8C8F-4A607B48E4BB
# 



library(tidyverse)
library(magrittr)

## read the data

ag_data <- read_csv("C:/Users/dell/Desktop/berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
colnames(ag_data)[bb]

## list the 1-unique single values.  
## Consider if they should be used for labels

single_values <- ag_data[1,bb]
single_values


## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))



## Make a table of the number of unique values in each column.
aa %<>% select(-all_of(bb)) 

## State name and the State ANSI code are (sort of) redundant
## Just keep the name
## Note: There may be times when having the numeric code will be handy)

ag_data %<>% select(-4)
aa %<>% select(-4) 

## Now list the values in the columns that
aa
# A tibble: 1 x 8
# Year Period State Commodity `Data Item` Domain `Domain Category` Value
# <int>  <int> <int>     <int>       <int>  <int>             <int> <int>
#   1     5      3    18         3         106      6               269  2362



ag_data$Year %>%  unique()
## [1] 2015 2018 2017 2016 2015

ag_data$Period %>% unique()
## "MARKETING YEAR"      "YEAR"                "YEAR - AUG FORECAST"

## Year: 
## Generally refers to calendar year. 
## For Prices Received data, refers to 
## an unweighted average (by month) for the calendar year. 

## Marketing year:
## Definition varies by commodity; 
## see Agricultural Prices publications
## for definitions by commodity. 
## For Prices Received data, refers to a
## weighted average for the marketing year.



ulst <- lapply(ag_data[,(which(aa[1,]<100))], unique)

lapply(ulst, t)

ulst



########################################################################
### let's focus on: period = "Year" and Commodity = "RASPBERRIES"

## RASPBERRY data
ag_data_rb <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))


################################## separate here -- 
#ag_data_rb$`Data Item`

## separate ag_data_rb$`Data Item`  into "berry", "type", "data_type"

## ag_data_rb %>% separate(`Data Item`, c("berry", "type", "data_item"), ",")
  
# head(ag_data$`Data Item` %>% unique())


ag_data_rb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ", ") # changed
  
#######################################################
### 

nana <- lapply(ag_data_rb, is.na)


lapply(nana, sum)

ag_data_rb[is.na(ag_data_rb)] <- ""


head(ag_data_rb)

ag_data_rb %>% summarize_all(n_distinct) -> cnt_per_col

cnt_per_col

ag_data_rb[1,]

delcol <- c("Period", "Commodity", "berry")

ag_data_rb %<>%  select(-delcol)

# save.image("./01.Rdata")

# load("./01.Rdata")
str(ag_data_rb)
View(ag_data_rb[1:30,])

## period is "YEAR"
## Commodity is "RASPBERRIES"
## berry is "RASPBERRIES"

## Year is 2015 2018 2017 2016 2015
## State is [1] "CALIFORNIA"     "FLORIDA"        "GEORGIA"        "MAINE"          "MICHIGAN"      
##          [6] "NEW JERSEY"     "NORTH CAROLINA" "OREGON"         "OTHER STATES"   "WASHINGTON"    
##          [11] "MISSISSIPPI"    "NEW YORK"       "ALABAMA"        "ARKANSAS"       "INDIANA"   

######################
# ag_data_bb$type <-  str_split(ag_data_bb$type, " - ", simplify = TRUE)
# ag_data_bb$type
# ag_data_bb %>% separate(type, into=c("kind", "measure")) 
# 
# ag_data_bb %<>% separate(type, into=c("kind", "measure")) 
# 
# 
# sum(is.na(ag_data_bb))
# 
# ag_data_bb[is.na(ag_data_bb)] <- ""
# 
# 
# 
# #ag_data_bb %<>% separate(ag_data_bb, separate(type, c("type", "measure"), sep = " - ", extra = "warn"))
# 
# ag_data_bb_a <- ag_data_bb
# 
# ag_data_bb_a <- mutate(kind = str_split(ag_data_bb_a$type, " - ", simplify = TRUE))
# 
# unique(ag_data_bb_a$kind)
# 
# 
# ag_data_bb_a$type <- str_split(ag_data_bb_a$type, " - ", simplify = TRUE)
# ag_data_bb %>% separate(type, into=c("kind", "measure")) 
# head(ag_data_bb_a)


# -----------------------------------------------------

View(ag_data_rb[1000:1030,])
ag_data_rb %<>% separate(type, c("kind", "measure"), " - ")
str(ag_data_rb)

# save.image("./02.Rdata")
# load.image("./02.Rdata")

View(ag_data_rb[1:50,])


# convert Value into numeric
ag_data_rb %<>% 
  mutate(ValueNum = str_replace_all(Value, ",", "")) %>%
  mutate(ValueNum = as.integer(ValueNum))

View(ag_data_rb[1:100,])


# --------- interested variables -------

measure_v1 = c("ACRES HARVESTED", "PRODUCTION", "YIELD")
data_item_v1 = c("FRESH MARKET - PRODUCTION","NOT SOLD - PRODUCTION","PROCESSING - PRODUCTION","UTILIZED - PRODUCTION")
ag_data_rb %>% 
  filter( (measure %in% measure_v1) | (data_item %in% data_item_v1) ) -> ag_data_rb_v1

View(ag_data_rb_v1)

# save.image("./03.Rdata")
# load("./03.Rdata")


# spread
# ag_data_rb_v1 %>%
#   spread(key = data_item, value = ValueNum) %>% View()



ag_data_rb_v1 %<>% 
  select(Year,State,kind,measure,data_item,unit,Value,ValueNum)

# combine data_item and unit
ag_data_rb_v1 %>%
  mutate(data_item_unit = paste(data_item, unit)) %>% 
  select(-data_item, -unit, -Value, -kind) -> ag_data_rb_v2

View(ag_data_rb_v2)

ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[1] ) %>% View()

ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[2], data_item_unit %in% "MEASURED IN LB " ) %>% View()


ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[3] ) %>% View()


# extract value

tmp2 = ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[2], data_item_unit %in% "MEASURED IN LB ") %>%
  select(Year, State, measure, ValueNum) %>%
  filter( !is.na(ValueNum) ) %>%
  group_by(Year, State, measure) %>%
  summarise(ValueNum = sum(ValueNum)) %>%
  rename(PRODUCTION = ValueNum)

tmp1 = ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[1] ) %>%
  select(Year, State, measure, ValueNum) %>%
  group_by(Year, State, measure) %>%
  summarise(ValueNum = sum(ValueNum)) %>%
  rename(ACRES_HARVESTED = ValueNum)

View(tmp1); View(tmp2)

tmp3 = ag_data_rb_v2 %>%
  filter( measure %in% measure_v1[3] ) %>%
  select(Year, State, measure, ValueNum) %>%
  rename(YIELD = ValueNum)

View(tmp3)

full_join(tmp1[,-3], tmp2[,-3]) %>% full_join(tmp3[,-3]) -> dat2


# --------- Data Analysis ------------

View(dat2)

# save.image("./04.Rdata")
# save(list = c("dat1", "dat2"), file = "./04.Rdata")


# ACRES_HARVESTED in Acre in 2015
dat2 %>% 
  filter(Year == 2015) %>%
  group_by(State) %>%
  arrange(desc(ACRES_HARVESTED)) %>%
  select(-PRODUCTION, -YIELD)

# ACRES_HARVESTED in Acre top 3
dat2 %>% 
  group_by(Year) %>%
  # arrange(desc(ACRES_HARVESTED)) %>%
  top_n(3, ACRES_HARVESTED) %>%
  select(-PRODUCTION, -YIELD)

# plot
dat2 %>% 
  filter(Year == 2015) %>%
  ggplot(aes(x = State, y = ACRES_HARVESTED)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Area of Harvested in 2015")

dat2 %>% 
  ggplot(aes(x = Year, y = ACRES_HARVESTED, color = State)) +
  geom_line() +
  labs(title = "Area of Harvested of the last 5 years")



# PRODUCTION (in lbs) in 2015
dat2 %>% 
  filter(Year == 2015) %>%
  group_by(State) %>%
  arrange(desc(PRODUCTION)) %>%
  select(-YIELD, -ACRES_HARVESTED)


# PRODUCTION (in lbs)  top 3
dat2 %>% 
  group_by(Year) %>%
  top_n(3, PRODUCTION) %>%
  select(-YIELD, -ACRES_HARVESTED)


# plot
dat2 %>% 
  filter(Year == 2015) %>%
  ggplot(aes(x = State, y = PRODUCTION)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "PRODUCTION (in lbs) in 2015")

dat2 %>% 
  ggplot(aes(x = Year, y = PRODUCTION, color = State)) +
  geom_line() +
  labs(title = "PRODUCTION (in lbs) of the last 5 years")




# YIELD in  lbs per Acre  in 2015
dat2 %>% 
  filter(Year == 2015) %>%
  group_by(State) %>%
  arrange(desc(YIELD)) %>%
  select(-PRODUCTION, -ACRES_HARVESTED)


# YIELD (in lbs per Acre) top 3
dat2 %>% 
  group_by(Year) %>%
  top_n(3, YIELD) %>%
  select(-PRODUCTION, -ACRES_HARVESTED)


# plot
dat2 %>% 
  filter(Year == 2015) %>%
  ggplot(aes(x = State, y = YIELD)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "YIELD (in lbs per Acre) in 2015")

dat2 %>% 
  ggplot(aes(x = Year, y = YIELD, color = State)) +
  geom_line() +
  labs(title = "YIELD (in lbs per Acre) of the last 5 years")

# It is very interesting that 3 States (CALIFORNIA, OREGON, WASHINGTON) 
# has much higher YIELD(in lbs per Acre) than the other states. 
# Further study may be needed



# map

library(usmap)

plot_usmap(regions = "states")
str(statepop)

dat2 %>%
  filter(Year == 2015) %>%
  select(State, YIELD) %>%
  mutate(State = str_to_title(State)) %>%
  right_join(statepop, by = c("State" = "full")) -> dat3
  

plot_usmap(data = dat3, regions = "states", values = "YIELD", color = "white") + 
  scale_fill_continuous(name = "YIELD", low = "white", high = "red") +
  theme(legend.position = "right")

# It can be seen that the three States have much higher YIELD are 
# all located in the **West Coast**. 
# The weather may be a great reason of the high YIELD.

save(list = c("dat2", "dat3"), file = "./04_rasp.Rdata")



