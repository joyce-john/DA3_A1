#############################################################
############# VIENNA AIRBNB CLEANING SCRIPT #################
#############################################################


############################
######     SETUP      ######
############################



# load libraries
library(tidyverse)

# SET WORKING DIRECTORY  TO YOUR PREFERRED PATH
dir <- getwd()

# location folders
data_in  <- paste0(dir,"/data/raw/")
data_out <- paste0(dir,"/data/clean/")

# load data
df <- read_csv(paste0(dir,'/data/raw/listings.csv.gz'))



############################
######      CLEAN     ######
######    FEATURES    ######
######        &       ######
######     DISCARD    ######
######    JUNK OBS    ######
############################



# 'Entire apartment' + 'Private room in apartment' are 87% of obs, 
# there are 59 other types, and each is <= 2.3% of total obs
proptypes <- 
  df %>% 
  group_by(property_type) %>%  
  summarize(count = n())

# we can gather all the different permutations of 'apartment' with string detection
# and discard property types that aren't apartments, because they are a small number of obs and...
# ...OUR CLIENT IS ONLY INTERESTED IN APARTMENTS ANYWAY
df <-
  df %>% 
  mutate(property_type = ifelse(str_detect(property_type, 'apartment'), 'Apartment', property_type)) %>% 
  mutate(property_type = ifelse(!str_detect(property_type, 'Apartment'), 'Other', property_type))

# keep only apartments
df <- 
  df %>%
  filter(property_type == 'Apartment')

# look at roomtypes
# apartments are not hotels and do not have hotel rooms, we should discard 'Hotel room' obs
table(df$room_type)

# discard hotel rooms with a negated filter
df <-
  df %>% 
  filter(!room_type == 'Hotel room')

# rename 'Entire home/apt' to 'Entire apartment'
df <-
  df %>% 
  mutate(room_type = ifelse(room_type == 'Entire home/apt', 'Entire apartment', room_type))



# consider how to handle bathroom_text column. first look at potential values
table(df$bathrooms_text)

# we can just drop the obs with "0 bathrooms", it's only 26
df <- 
  df %>% filter(bathrooms_text != '0 baths' & bathrooms_text != '0 shared baths')

# looks turn all other values into numbers and add a flag for 'shared bathoom'
df <-
  df %>% 
  mutate(bathrooms = str_extract(bathrooms_text, '\\d\\.*\\d*')) 

# for some reason, I have to do this as a separate step: detect half-baths and fill numeric values
df <-
  df %>% 
  mutate(bathrooms = ifelse(str_detect(bathrooms_text, 'Half-bath|half-bath'), 0.5, bathrooms))
 
# add flag for shared bathrooms
df <- 
  df %>% 
  mutate(shared_bathroom = ifelse(str_detect(bathrooms_text, 'shared'), 1, 0)) 
  
  
# filter to include apartments which meet our criteria: accomodates 2-6 people
df <- 
  df %>% 
  filter(accommodates %in% 2:6)



############################
######      CLEAN     ######
######   Y VARIABLE   ######
######      PRICE     ######
############################



# let's take a look at our target variable, price

# strip dollar signs from price column by taking substring starting at second position
df <-
  df %>% 
  mutate(price = str_sub(price, start = 2))

# convert to numeric
df$price <- as.numeric(df$price)

# check for NAs, it turns out there are 19. small number, let's drop them
sum(is.na(df$price))

# drop price NAs
df <-
  df %>% 
  filter(!is.na(price))

# visualize price histogram
df %>% 
  ggplot(aes(x = price)) + 
  geom_histogram()

# we should drop extreme values to improve our predictions
# but also because luxury apartments are not relevant comparisons for our business case
df <-
  df %>% 
  filter(price < 600)

# based on the distribution (long right tail), let's create a log price variable
df <-
  df %>% 
  mutate(ln_price = log(price))



############################
######    EXPLORE     ######
######    NUMERICS    ######
######  ASSOCIATION   ######
######      WITH      ######
######      PRICE     ######
############################



# look at relationships between meaningful numerics and price

# accomodates
df %>% 
  ggplot(aes(x = accommodates, y = price)) +
  geom_point(alpha = 0.5)

# bathrooms
df %>% 
  ggplot(aes(x = bathrooms, y = price)) +
  geom_point(alpha = 0.5)

# bedrooms - our warning reveals that there are 1131 NAs
df %>% 
  ggplot(aes(x = bedrooms, y = price)) +
  geom_point(alpha = 0.5)

# beds
df %>% 
  ggplot(aes(x = beds, y = price)) +
  geom_point(alpha = 0.5)

# reviews
df %>% 
  ggplot(aes(x  = number_of_reviews, y = price)) +
  geom_point(alpha = 0.5)

# reviews: how about with logs? - doesn't look like a meaningful pattern of association...
# but it's a very crowded scatterplot... can't really tell if there are any patterns in there 
# I'll take log of number_of_reviews on account of its skewed distribution 
# and see if it helps during model evaluation phase
df %>% 
  ggplot(aes(x = log(number_of_reviews), y = log(price))) +
  geom_point(alpha = 0.5)

# I will not investigate reviews numeric variables further, because they will not be useful to our business case
# the client is introducing new apartments to the market which will not have any reviews...
# and we don't know what the clients review scores will be (overall, cleanliness, etc)


# take log of number of reviews, but no further transformations of numeric variables necessary
# add a small contant to number_of_reviews to avoid getting -Inf for obs of 0
df <-
  df %>% 
  mutate(ln_num_reviews = log(number_of_reviews + 0.01))



############################
######    FEATURE     ######
######  ENGINEERING:  ######
######  BINARY VARS   ###### 
######  FOR AMENITIES ######
############################
  
  

# drop a bunch of variables we won't use to make wrangling amenities a little easier
df <- 
  df %>% 
  select(id, 
         name, 
         neighbourhood_cleansed, 
         latitude, 
         longitude, 
         room_type, 
         accommodates,
         bathrooms,
         bedrooms,
         beds,
         amenities,
         price,
         instant_bookable,
         number_of_reviews,
         shared_bathroom,
         ln_price,
         ln_num_reviews)


# creating dummy variables for amenities

# first create unique list of all amenities in the data
amenities_unique <- 
  unique(
    unlist(
      df$amenities %>% str_extract_all('(?<=")(\\w+\\s*\\w*\\s*\\w*)(?=")')
    )
  )

# then iterate over the list and create a new column for each unique amenity and fill values with this logic:
# in the new column, if the amenity is detected as a string in the original "amenities" at this row, set value to 1
# else set value to 0
for (i in amenities_unique){
  df <- 
    df %>% 
    mutate(!! i := ifelse(str_detect(amenities, !! i), 1, 0))
}

# mutate columns to aggregate refrigerators, ovens, stove of different types
# this checks for any type of refrigerator, oven or stove unit and sets value to 1 if present
# example: Refrigerator = 1 if it is a refrigerator, Siemens refrigerator, Gorenje refrigerator, Miele refrigerator, etc...
df <-
  df %>% 
  mutate(Refrigerator = ifelse(str_detect(amenities, "Refrigerator|refrigerator"), 1, 0)) %>% 
  mutate(Oven = ifelse(str_detect(amenities, "Oven|oven"), 1, 0)) %>% 
  mutate(Stove = ifelse(str_detect(amenities, "Stove|stove"), 1, 0)) %>% 
  mutate(`Body Soap` = ifelse(str_detect(amenities, "Body soap|body soap|Shower gel"), 1, 0)) %>% 
  mutate(Shampoo = ifelse(str_detect(amenities, "Shampoo|shampoo"), 1, 0)) %>% 
  mutate(Conditioner = ifelse(str_detect(amenities, "Conditioner|conditioner"), 1, 0))
  mutate(Netflix = ifelse(str_detect(amenities, "Netflix|netflix"), 1, 0)) %>% 
  mutate(`Air conditioning` = ifelse(str_detect(amenities, "Air conditioning|air conditioning|Window AC"), 1, 0)) %>% 
  mutate(TV = ifelse(str_detect(amenities, "TV"), 1, 0)) %>% 
  mutate(`Hot tub` = ifelse(str_detect(amenities, "Hot tub|hot tub"), 1, 0))


# drop aggregated vars we don't need anymore

# make vector of vars to drop
# decided to keep brand-name cofee machines (Keurig and Nespresso) because some people really like those
# in a way I don't think people care about the brand of the oven or refrigerator in an Airbnb
vars_to_drop <- c('Cable TV', 
                  'Shower gel', 
                  'Zanussi oven', 
                  'Zanussi electric stove', 
                  'Zanussi refrigerator',
                  'Window AC unit',
                  'Electric stove',
                  'HDTV with Netflix',
                  'Central air conditioning',
                  'Nivea body soap',
                  'Bodyshop body soap',
                  'Stainless steel oven',
                  'TV with Netflix',
                  'HDTV',
                  'Induction stove',
                  'SIEMENS oven',
                  'AEG refrigerator',
                  'Duschgel body soap',
                  'Private hot tub',
                  'Gorenje induction stove',
                  'Gorenje refrigerator',
                  'Portable air conditioning',
                  'Cerankochfeld electric stove',
                  'Siemens oven',
                  'Siemens refrigerator',
                  'Oranier induction stove',
                  'Rituals body soap',
                  'Rituals shampoo',
                  'variouse body soap',
                  'various conditioner',
                  'Bauknecht refrigerator',
                  'TV with Chromecast',
                  'Miele refrigerator',
                  'amenities') # we don't need this column anymore

# drop the vars using the vector 
df <- 
    df %>% 
    select(-vars_to_drop)



############################
######      SAVE      ######
######   CLEAN CSV    ######
############################



# write out a clean CSV
write_csv(df, paste0(data_out,'airbnb_vienna_midsize_clean.csv'))