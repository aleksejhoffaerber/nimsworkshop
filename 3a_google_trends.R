# This is a R script - each line is code that is interpreted by a computer
# Comments after an "#" are not interpreted - used to make scripts readable


## Step 0: Data import
# Everything is based on some kind of data to work with

# R Studio (this IDE) helps with the different commands
?read.csv

# Import local file from working directory 

## IMPORTANT: THIS DOES NOT WORK IF YOU DON'T 
## HAVE THE SAME DOCUMENT SAVED LOCALLY !!

nims <- read.csv("data_nims.csv", 
                 header = T)


# Store something in objects, e.g. the 4-letter-combination "nims"
# Meet the dataframe
nims

mean(nims$Average_Temperature)


#### ------------  Google Trends ------------ ####

## Step 1: Include an external package
install.packages("gtrendsR") 
require(gtrendsR)

## Step 2: make a query "to Google": copy-paste from documentation
gtrends(
  keyword = c("flower","plant","online delivery"), # <-- change this
  geo = "GB",
  time = "2020-01-01 2020-04-30",
  gprop = c("web", "news", "images", "froogle", "youtube"),
  category = 0,
  hl = "en-US",
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0,
  onlyInterest = FALSE
) -> flower_trend


## Step 3: visualize the result (= plot it)
x <- plot(flower_trend)
x


## Step 4: why R? Analyze it further

# 4a) Look into queries
flower_trend$related_queries$value 

# 4b) Fancier visualization
require(ggplot2)
require(dplyr)

flower_trend %>%
  .$interest_over_time %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword) +
  ggthemes::theme_economist()

# 4c) Look into further options:
# Inspect various countries for Google queries

data(countries)
grep("England", countries$name, ignore.case = T)
countries[1274,] # Code: GB
