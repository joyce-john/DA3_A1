################################################################################
###################                   VIENNA                ####################
###################               MID-SIZE AIRBNB           ####################
###################              PRICE PREDICTION           ####################
################################################################################



# This script uses a function written by Gabor Bekes and Gabor Kezdi for the code which accompanies their book:
# Data Analysis for Business, Economics, and Policy
# The function is called price_diff_by_variables
# Check out: gabors-data-analysis.com and github.com/gabors-data-analysis/
# Thank you Gabor for making Zoom class fun!



################################################################################
###################                                         ####################
###################              PREPARE DATA               ####################
###################                                         ####################
################################################################################



# load libraries
library(tidyverse)
library(caret)


# SET WORKING DIRECTORY  TO YOUR PREFERRED PATH
dir <- getwd()

# github URL for loading data from my repo
github_url <- 'https://raw.githubusercontent.com/joyce-john/DA3_A1/main/data/clean/'

# location folders - assuming you are downloading from GitHub and not using any special folder structure when running
data_in <- github_url
data_out <- dir

# # location folders - for local work
# data_in  <- paste0(dir,"/data/clean/")
# data_out <- paste0(dir,"/data/clean/")


# read the data into a dataframe
df <- read_csv(paste0(data_in, 'airbnb_vienna_midsize_clean.csv'))


# take a look at the situation with NAs
# all NAs were dealt with in the cleaning script, except for bedrooms
na_info <- df %>% select(-c('id', 'name')) %>% summarise_all(funs(sum(is.na(.))))
na_info[,colSums(na_info>0)>0]


# change some features to factors
df <- 
  df %>% 
  mutate_if(is.character, factor) %>% 
  mutate(bedrooms = factor(bedrooms, levels = c(1, 2, 3, 4, 5, NA), exclude = NULL)) # setting to factor to allow use of NAs in ML



################################################################################
###################              FUNCTIONAL FORM            ####################
###################              CONSIDERATIONS:            ####################
###################           UNCOMMENT TO SEE CHARTS       ####################
################################################################################



# ##############################################################################
# 
# # THIS SECTION COMMENTED OUT SO THE SCRIPT CAN RUN FASTER
# # SUMMARY: no evidence to suggest that we should alter the functional form of the variables
# 
# 
# # Look at some scatterplots to answer key questions about functional form...
# # Should we take log of our y variables, price, because its distribution is skewed?
# # Should we transform any of our other x variables (log, quadratic, cubic, whatever)...
# # ...to better accommodate a certain pattern of association with price or log price?
# 
# # number of reviews and price
# df %>% ggplot(aes(x = number_of_reviews, y = price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # number of reviews and **log price**
# df %>% ggplot(aes(x = number_of_reviews, y = ln_price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # **log number of reviews** and price
# df %>% ggplot(aes(x = ln_num_reviews, y = price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # **log number of reviews** and **log price**
# df %>% ggplot(aes(x = ln_num_reviews, y = ln_price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # accommodates and price
# df %>% ggplot(aes(x = accommodates, y = price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # accommodates and **log price**
# df %>% ggplot(aes(x = accommodates, y = ln_price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # bathrooms and price
# df %>% ggplot(aes(x = bathrooms, y = price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # bathrooms and **log price**
# df %>% ggplot(aes(x = bathrooms, y = ln_price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # bedrooms and price
# df %>% ggplot(aes(x = as.numeric(bedrooms), y = price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # bedrooms and **log price**
# df %>% ggplot(aes(x = as.numeric(bedrooms), y = ln_price)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
# 
# # After looking at these graphs, my answer is "no". 
# # There are some quadratic-looking patterns here, like price/ln_price & bathrooms. But it's convex! 
# # That doesn't seem to make sense, and I think its due to the lower number of "4 bathrooms" obs.
# 
# # Okay, just one more consideration: should I at least make a binary variable "no_reviews"
# # to capture any differences in price between brand new apartments and well-established ones?
# 
# # create a no_reviews binary variable, and compare mean prices
# df %>% 
#   mutate(no_reviews = ifelse(number_of_reviews == 0, TRUE, FALSE)) %>% 
#   ggplot(aes(x = no_reviews, y = price)) + 
#   geom_jitter(color="black", size=0.2, alpha=0.3) +
#   geom_boxplot()
# 
# # It's not what I expected to find...
# # the boxplot doesn't suggest that zero reviews has a special association with price.
# 
# ##############################################################################



################################################################################
###################                                         ####################
###################          CONSIDER INTERACTIONS          ####################
###################                                         ####################
################################################################################



# SUMMARY: parking variables definitely interact with neighbourhood in a meaningful way
# CONT... I'm not 100% confident in the others; I will include them and let LASSO decide

# define a function to create plots which help identify potential interactions
# this function was written by Gabor Bekes and  Gabor Kezdi and modified slightly
price_diff_by_variables <- function(df, factor_var, dummy_var){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9))+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    ylab('Mean Price')+
    theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #John's modification
    labs(x = 'Neighbourhood') + # John's modification
    scale_fill_grey()
}

# now, let's consider whether there any interactions with neighbourhood and other variables
# take a look at interactions with some of the basic listing information variables
# I can't examine all 100+ amenities, but domain knowledge suggests we should consider PARKING

#Look up parking (free/paid) interactions with neighbourhood
p1 <- price_diff_by_variables(df, "neighbourhood_cleansed", "Free_parking")
p2 <- price_diff_by_variables(df, "neighbourhood_cleansed", "Paid_parking")

# room_type?
p3 <- price_diff_by_variables(df, "neighbourhood_cleansed", "room_type" )

# accommodates?
p4 <- price_diff_by_variables(df, "neighbourhood_cleansed", "accommodates")

# instant bookable?
p5 <- price_diff_by_variables(df, "neighbourhood_cleansed", "instant_bookable")

# these plots are crowded, so I examine them one-by-one
p1
p2
p3
p4
p5

# Without examining *all* the variable interactions...
# I can see that some basic variables interact with neighbourhood but others don't (shocker)
# There is no way I could test all the amenities by hand, and there is no point in thinking too hard
# about which interactions to include and which ones to throw away, when LASSO can make this decision for me
# I will throw a bunch of neighbourhood* interactions into the mix, because *some* of them will be useful
# and the others will get zero'd out by LASSO



################################################################################
###################                  PREPARE                    ################
###################                  FORMULAS                   ################
###################              MODEL PARAMETERS               ################
################################################################################



########################
#####   FORMULAS    ####
########################


# put variables in groups to make model-building easier

# basic characteristics of the listing
basic_vars <- c('neighbourhood_cleansed', 
                'room_type', 
                'accommodates', 
                'bathrooms', 
                'bedrooms', 
                'beds', 
                'instant_bookable', 
                'shared_bathroom',
                'number_of_reviews')

# interactions with neighbourhood
# I can't explore all possible interactions, so I'm only using basic property stats + parking
neighbourhood_interactions <- c('room_type*neighbourhood_cleansed', 
                                'accommodates*neighbourhood_cleansed', 
                                'bathrooms*neighbourhood_cleansed', 
                                'bedrooms*neighbourhood_cleansed', 
                                'beds*neighbourhood_cleansed', 
                                'instant_bookable*neighbourhood_cleansed', 
                                'shared_bathroom*neighbourhood_cleansed',
                                'number_of_reviews*neighbourhood_cleansed',
                                'Free_parking*neighbourhood_cleansed',
                                'Paid_parking*neighbourhood_cleansed')

# dummy variables for the amenities (fridges, parking, etc.) in columns 15 - 123
amenities <- colnames(df[15:123])


# simplify model formulas with these groups
predictors_1 <- basic_vars
predictors_2 <- c(basic_vars, amenities)
predictors_3 <- c(basic_vars, amenities, neighbourhood_interactions)


# set formulas

# formula 1: basic variables
formula_1 <- 
  as.formula(
    paste0('price ~ ', 
           paste0(predictors_1,
                  collapse = ' + ')))

# formula 2: basic variables + amenities
formula_2 <- as.formula(
  paste0('price ~ ', 
         paste0(predictors_2,
                collapse = ' + ')))

# formula 3: basic variables + amenities + neighbourhood interactions
formula_3 <- as.formula(
  paste0('price ~ ', 
         paste0(predictors_3,
                collapse = ' + ')))


################################
#####   MODEL PARAMETERS    ####
################################


# set trainControl to do 5 folds of cross validation
train_control <- trainControl(method = "cv", number = 5)

# tuneGrid for LASSO
# try lambdas all the way up 1, we need to allow LASSO to basically zero things out if appropriate
tunegrid_lasso <-  expand.grid("alpha" = 1, "lambda" = seq(0.05, 1, by = 0.01))

# set tunegrid for random forest: simple model
# for the simple model, we have 9 features. we may consider sqrt(9) = 3 as a potential value for m
# let's at least give it two options: 3 and 4. if we go any higher, the trees may get too correlated
tune_grid_rf_simple <- expand.grid(
  .mtry = c(3, 4),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

# set tunegrid for random forest: full model
# for the full model, we have 121 features, we may consider sqrt(121) = 11 as a possible value for m
# but let's have it try a few values
tune_grid_rf_full <- expand.grid(
  .mtry = c(9, 11, 13),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

# setting train control for RF - the same as previous train control but with verboseIter = TRUE
# so I can see the progress
train_control_rf <- trainControl(method = "cv", number = 5, verboseIter = TRUE)



################################################################################
###################                                             ################
###################               CREATE MODELS                 ################
###################                                             ################
################################################################################



# OLS

# linear model 1: basic predictors
lm_1 <- caret::train(formula_1,
                    data = train_set,
                    method = 'lm',
                    preProcess = c('center', 'scale'),
                    trControl = train_control)

# linear model 2: basic predictors + amenities
lm_2 <- caret::train(formula_2,
                    data = train_set,
                    method = 'lm',
                    preProcess = c('center', 'scale'),
                    trControl = train_control)

# linear model 3: basic predictors + amenities + neighbourhood interactions
lm_3 <- caret::train(formula_3,
                    data = train_set,
                    method = 'lm',
                    preProcess = c('center', 'scale'),
                    trControl = train_control)

# LASSO

# give LASSO all potential x variables and let the smartypants algo decide what's important
lasso1 <- caret::train(formula_3,
                       data = train_set,
                       method = 'glmnet',
                       preProcess = c('center', 'scale'),
                       tuneGrid = tunegrid_lasso,
                       trControl = train_control
)

# Random Forest

# first train a simple model only using basic property stats
rf_1 <- train(
  formula_1,
  data = train_set,
  method = "ranger",
  trControl = train_control_rf,
  tuneGrid = tune_grid_rf_simple,
  importance = "impurity"
)

# second train a complex model using all features
rf_2 <- train(
  formula_3,
  data = train_set,
  method = "ranger",
  trControl = train_control_rf,
  tuneGrid = tune_grid_rf_full,
  importance = "impurity"
)



################################################################################
#################                                               ################
#################               EVALUATE MODELS                 ################
#################                                               ################
################################################################################



######################################
#####    CROSS-VALIDATED RMSE    #####
######################################


# put all models into a list
final_models <-
  list("OLS_1" = lm_1,
       "OLS_2" = lm_2,
       "OLS_3" = lm_3,
       "LASSO" = lasso_1,
       "Random_forest_basic" = rf_1,
       "Random forest_full" = rf_2)

# summarize model stats
results <- resamples(final_models) %>% summary()

# examine cross-validated RMSE
results$statistics$RMSE

# LASSO and Random Forest (full model) have the lowest mean cross-validated RMSE


######################################
#####       TEST-SET RMSE        #####
######################################


# make predictions  on the **test set**
predictions_lm_1 <- predict(lm_1, test_set)
predictions_lm_2 <- predict(lm_2, test_set)
predictions_lm_3 <- predict(lm_3, test_set)
predictions_lasso_1 <- predict(lasso_1, test_set)
predictions_rf1 <- predict(rf_1, test_set)
predictions_rf2 <- predict(rf_2, test_set)

# calculate RMSE for test set predictions
test_set_rmse <- data.frame('OLS_1' = RMSE(predictions_lm_1, test_set$price), 
                            'OLS_2' = RMSE(predictions_lm_2, test_set$price),
                            'OLS_3' = RMSE(predictions_lm_3, test_set$price),
                            'LASSO' = RMSE(predictions_lasso_1, test_set$price),
                            'Random_forest_basic' = RMSE(predictions_rf1, test_set$price),
                            'Random_forest_full' = RMSE(predictions_rf2, test_set$price))

# LASSO and Random Forest (full model) still have the best RMSE
# and it's similar to the training set RMSE, which is a good sign that we didn't overfit
print(test_set_rmse)

# The winner? Random forest (full model) for having the lowest RMSE.


##################################################
#####        DIAGNOSTICS: Y-hat VS Y         #####
##################################################

# LASSO - scatterplot of predicted VS actual values
lasso_pred_vs_actual <- 
  test_set %>% 
  mutate(predictions = predictions_lasso_1) %>% 
  ggplot(aes(x = predictions, y = price)) +
  geom_point(color = 'blue', size = 1, shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color = 'red', linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(title = 'LASSO', y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme(plot.title = element_text(hjust = 0.5))

# RANDOM FOREST (full) - scatterplot of predicted VS actual values
rf_2_pred_vs_actual <-
  test_set %>% 
  mutate(predictions = predictions_lasso_1) %>% 
  ggplot(aes(x = predictions, y = price)) +
  geom_point(color = 'blue', size = 1, shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color = 'red', linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(title = 'Random Forest (full model)', y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme(plot.title = element_text(hjust = 0.5))

# show LASSO scatter
lasso_pred_vs_actual

# show RF scatter
rf_2_pred_vs_actual


##########################################################
#####        DIAGNOSTICS: LASSO COEFFICIENTS         #####
##########################################################


# get the coefficients from the LASSO model
lasso_coeffs <- coef(lasso_1$finalModel, lasso_1$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

# NOT RUN: print out all the coefficients if you want to gaze at all the 0s
#print(lasso_coeffs)

# count the coefficients which are non-zero
lasso_coeffs_nz <- lasso_coeffs %>% filter(coefficient!=0)

# there are 184 non-zero coefficients
print(nrow(lasso_coeffs_nz))

# get the top 10 LASSO coefficients
lasso_top10_coef <-
  lasso_coeffs %>% 
  arrange(desc(coefficient)) %>% 
  head(., n = 10)

# view the top 10 LASSO coefficients
lasso_top10_coef

# some of these make perfect sense such as accomoodates, TV, Air_conditioning
# but it's surprising to see that Innere Stadt bathrooms could be so significant


#########################################################################
#####        DIAGNOSTICS: RANDOM FOREST VARIABLE IMPORTANCE         #####
#########################################################################


# calculate random forest variable importance as a percentage
rf_2_var_imp <- importance(rf_2$finalModel)/1000
rf_2_var_imp_df <-
  data.frame(varname = names(rf_2_var_imp), imp = rf_2_var_imp) %>%
  mutate(varname = gsub("room_typePrivate room", "room type: private room", varname) ) %>%
  mutate(varname = gsub("bedrooms2", "bedrooms: 2", varname) ) %>%
  mutate(varname = gsub("neighbourhood_cleansedInnere Stadt:bathrooms", "Innere Stadt * bathrooms", varname) ) %>%
  mutate(varname = gsub("neighbourhood_cleansedInnere Stadt:accommodates", "Innere Stadt * accommodates", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

# make plot of top 10 varimp vars
rf_2_var_imp_plot <- ggplot(rf_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color= 'blue', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color= 'blue', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  labs(title = "Top 10 Variables (VarImp)") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),
        plot.title = element_text(hjust = 0.5))

# show plot - it's tempting to group all the bathroom vars, but I don't want to hide the message
# number of bathrooms matters, so does whether it's shared or not, bathrooms in the Innere Stadt are special
rf_2_var_imp_plot


#########################################################################
#####         DIAGNOSTICS: RANDOM FOREST PARTIAL DEPENDENCE          ####
#########################################################################


# partial dependence plot for number of people it accommodates
pdp_n_acc <- pdp::partial(rf_2, pred.var = "accommodates", pred.grid = distinct_(test_set, "accommodates"), train = train_set)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color= 'blue', size=2) +
  geom_line(color= 'red', size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  labs(title = "Partial Dependence Plot - Accommodates")
scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1)) +
  theme(plot.title = element_text(hjust = 0.5))

# show the plot
pdp_n_acc_plot


# partial dependence plot for number of bathrooms
pdp_bathrooms <- pdp::partial(rf_2, pred.var = "bathrooms", pred.grid = distinct_(test_set, "bathrooms"), train = train_set)
pdp_bathrooms_plot <- pdp_bathrooms %>%
  autoplot( ) +
  geom_point(color= 'blue', size = 2) +
  geom_line(color= 'red', size = 1) +
  ylab("Predicted price") +
  xlab("Number of Bathrooms") +
  scale_x_continuous(limit=c(0.5,4), breaks=seq(0.5,4,0.5)) +
  labs(title = "Partial Dependence Plot - Bathrooms")+
  theme(plot.title = element_text(hjust = 0.5))

# show the plot
# th is is certainly an interesting shape
# finding patterns like this is the strength of machine learning
pdp_bathrooms_plot