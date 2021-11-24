#### Food Security 
### Logistic Regression Decision Tree and Random Forest

library(readr)
library(tidyverse)
library(forcats)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
set.seed(42)

### Load the data
raw.foodsec1 <- read.csv('/Users/jacobmorgan/Desktop/Jobs/2021/Analyses/FoodSecurity/cchs-82M0013-E-2015-2016-Annual-component_F1.csv')
raw.foodsec2 <- read.csv('/Users/jacobmorgan/Desktop/Jobs/2021/Analyses/FoodSecurity/cchs-82M0013-E-2017-2018-Annual-component_F1.csv')

### Pre-processing
## Rename Columns
# Select columns of interest
# Ensure both datasets have the same column names

raw.foodsec1 <- raw.foodsec1 %>%
              rename(
                  Province = GEO_PRV,
                  Sex = DHH_SEX,
                  Age = DHHGAGE,
                  LiveArrange = DHHDGLVG,
                  Education = EHG2DVH3,
                  PerceivedHealth = GEN_005,
                  PerceivedMental = GENDVMHI,
                  SmokesDaily = SMK_015,
                  DrinkingRisk = ALWDVLTR,
                  HealthCare = PHC_020,
                  OwnHouse = DHH_OWN,
                  BirthCountry = SDCDGCB,
                  White = SDCDGCGT,
                  FoodSec = FSCDVHFS,
                  HouseInc = INCDGHH,
                  Weed = DRGDVYCM,
                  Drugs = DRGDVYAC,
                )

raw.foodsec2 <- raw.foodsec2 %>%
  rename(
    Province = GEO_PRV,
    Sex = DHH_SEX,
    Age = DHHGAGE,
    LiveArrange = DHHDGLVG,
    Education = EHG2DVH3,
    PerceivedHealth = GEN_005,
    PerceivedMental = GENDVMHI,
    SmokesDaily = SMK_015,
    DrinkingRisk = ALWDVLTR,
    HealthCare = PHC_020,
    OwnHouse = DHH_OWN,
    BirthCountry = SDCDGCB,
    White = SDCDGCGT,
    FoodSec = FSCDVHF2,
HouseInc = INCDGHH,
Weed = DRGDVYCM,
Drugs = DRGDVYAC,
)

## Drop and keep columns
raw.foodsec1 <-select(raw.foodsec1, c(FoodSec, Province, Sex, Age, LiveArrange, Education, PerceivedHealth, 
                            PerceivedMental, SmokesDaily, Weed, Drugs, DrinkingRisk, HealthCare, 
                            White, BirthCountry, OwnHouse, HouseInc))         

raw.foodsec2 <-select(raw.foodsec2, c(FoodSec, Province, Sex, Age, LiveArrange, Education, PerceivedHealth, 
                                      PerceivedMental, SmokesDaily, Weed, Drugs, DrinkingRisk, HealthCare, 
                                      White, BirthCountry, OwnHouse, HouseInc)) 

## Combine dataframes by row
foodsec <- rbind(raw.foodsec1, raw.foodsec2)

## Create factors of categorical variables
# Household food security status
foodsec$FoodSec <- foodsec$FoodSec %>%
  as.factor() %>% 
  fct_collapse(FoodSecure = '0', FoodInsecure = c('1', '2', '3'))
# Province
foodsec$Province <- foodsec$Province %>%
  as.factor() %>%
  fct_collapse(ND = '10', PEI = '11', NS = '12', NB = '13', QB = '24', 
               ON = '35', MT = '46', SA = '47', AB = '48', BC = '59', YK = '60',
               NW = '61', NV = '62')
# Sex of respondent
foodsec$Sex <- foodsec$Sex %>%
  as.factor() %>%
  fct_collapse(Male = '1', Female = '2')
# Age group
foodsec$Age <- foodsec$Age %>%
  as.factor() %>%
  fct_collapse('Under 20' = c('1','2','3'), '20 - 40' = c('4','5','6','7'), '40 - 65' = c('8','9','10','11','12'), '65+' = c('13','14','15','16'))
# Living Arrangement
foodsec$LiveArrange <- foodsec$LiveArrange %>%
  as.factor() %>%
  fct_collapse(Single = '1', SingleRoomies = '2', Partnered = '3', ThreeGenFam = '4', SingleParent = c('5','6'), MultiParent = '7')
# Education
foodsec$Education <- foodsec$Education %>%
  as.factor() %>%
  fct_collapse(Junior = '1', Secondary = '2', PostSecondary = '3')
# Perceived Health
foodsec$PerceivedHealth <- foodsec$PerceivedHealth %>%
  as.factor() %>%
  fct_collapse(Poor = '5', Fair = '4', Good = c('1','2','3'))
# Perceived Mental Health
foodsec$PerceivedMental <- foodsec$PerceivedMental %>%
  as.factor() %>%
  fct_collapse(Poor = '0', Fair = '1', Good = c('2', '3', '4'))
# Smokes Daily
foodsec$SmokesDaily <- foodsec$SmokesDaily %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Drinking Risk
foodsec$DrinkingRisk <- foodsec$DrinkingRisk %>%
  as.factor() %>%
  fct_collapse(Increased = '1', None = '2')
# Weed in the last year
foodsec$Weed <- foodsec$Weed %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Used drugs in the last year
foodsec$Drugs <- foodsec$Drugs %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# Healthcare Provider
foodsec$HealthCare <- foodsec$HealthCare %>%
  as.factor() %>%
  fct_collapse(Yes = '1', No = '2')
# White
foodsec$White <- foodsec$White %>%
  as.factor() %>%
  fct_collapse(White = '1', BIPOC = '2')
# Owns House
foodsec$OwnHouse <- foodsec$OwnHouse %>%
  as.factor() %>%
  fct_collapse(Owns = '1', Rents = '2')
# Birth Country
foodsec$BirthCountry <- foodsec$BirthCountry %>%
  as.factor() %>%
  fct_collapse(Canada = '1', Other = '2')
# Total Household Income bracket
foodsec$HouseInc <- foodsec$HouseInc %>%
  as.factor() %>%
  fct_collapse('<20k' = '1', '20k - 30k' = '2', '40k - 60k' = '3', '60k - 80k' = '4', '>80k' = '5')

## Filter out by missing / unused observations

foodsec <- foodsec %>%
  filter(
    (foodsec$FoodSec == 'FoodSecure' | foodsec$FoodSec == 'FoodInsecure') &
    (foodsec$Education == 'Junior' | foodsec$Education == 'Secondary' | foodsec$Education == 'PostSecondary') &
    (foodsec$LiveArrange == 'Single' | foodsec$LiveArrange == 'SingleRoomies' | foodsec$LiveArrange == 'Partnered' | foodsec$LiveArrange == 'ThreeGenFam' | foodsec$LiveArrange == 'SingleParent' | foodsec$LiveArrange == 'MultiParent') & 
    (foodsec$PerceivedHealth == 'Poor' | foodsec$PerceivedHealth == 'Fair' | foodsec$PerceivedHealth == 'Good') &
    (foodsec$PerceivedMental == 'Poor' | foodsec$PerceivedMental == 'Fair' | foodsec$PerceivedMental == 'Good') &
    (foodsec$SmokesDaily == 'Yes' | foodsec$SmokesDaily == 'No') &
    (foodsec$Weed == 'Yes' | foodsec$Weed == 'No') &
    (foodsec$Drugs == 'Yes' | foodsec$Drugs == 'No') &
    (foodsec$DrinkingRisk == 'Increased' | foodsec$DrinkingRisk == 'None' ) &
    (foodsec$HealthCare == 'Yes' | foodsec$HealthCare == 'No') &
    (foodsec$White == 'White' | foodsec$White == 'BIPOC') &
    (foodsec$BirthCountry == 'Canada' | foodsec$BirthCountry == 'Other') &
    (foodsec$OwnHouse == 'Owns' | foodsec$OwnHouse == 'Rents') &
    (foodsec$HouseInc == '<20k' | foodsec$HouseInc == '<20k - 40k' | foodsec$HouseInc == '40k - 60k' | foodsec$HouseInc == '60k - 80k' | foodsec$HouseInc == '>80k')
  ) %>%
  droplevels() # Remove unused factor levels

## Limit analysis to Ontario residents

foodsec <- foodsec %>%
  filter(foodsec$Province == 'ON')

## Balance the categories
# Randomly select 800 of the FoodSecure to compare against all (762) FoodInsecure

foodsec_FS <- foodsec %>%
                filter(FoodSec == 'FoodSecure') %>%
                  sample_n(., 800)
    
foodsec_FIs <- foodsec %>%
                filter(FoodSec == 'FoodInsecure')

foodsec.pre <- rbind(foodsec_FS, foodsec_FIs)

## Split the data into training and testing sets
foodsec_split <- rsample::initial_split(foodsec.pre, prop = 0.8)
foodsec_train <- rsample::training(foodsec_split)
foodsec_test <- rsample::testing(foodsec_split)

## Build the classification tree

foodsec_cart <- rpart(
  formula = FoodSec ~ ., 
  data = foodsec_train, 
  method = 'class',
  control = list(cp = 0.001)
)

printcp(foodsec_cart)
plotcp(foodsec_cart, upper = 'splits')
rpart.plot(foodsec_cart, yesno= T)

## Tune the model
# Create a tuning grid

tuninggrid <- expand.grid(
  minsplit = seq(4,20,1),
  maxdepth = seq(5,15,1)
)

# Create models according to the parameter values specified in the tuning grid
tuningmodels <- list()

for (i in 1:nrow(tuninggrid)) {
  # get the minsplit and maxdepth values of each row
  minsplit <- tuninggrid$minsplit[i]
  maxdepth <- tuninggrid$maxdepth[i]
  
  # train models based on minsplit and maxdepth, storing the values in the models list created globally
  tuningmodels[[i]] <- rpart(
    formula = FoodSec ~ .,
    data = foodsec_train,
    method = 'class',
    control = list(minsplit = minsplit, maxdepth = maxdepth, cp = 0.001)
  )
}

## Retrieve tuning values

# Optimal CP
get_cp <- function(x) {
  min <- which.min(x$cptable[, 'xerror'])
  cp <- x$cptable[min, 'CP']
}
# Minimum error
min_err <- function(x) {
  min <- which.min(x$cptable[ , 'xerror'])
  xerror <- x$cptable[min, 'xerror']
}

# Organize top 5 model parameters

tuninggrid %>%
  mutate(
    cp = purrr::map_dbl(tuningmodels, get_cp),
    error = purrr::map_dbl(tuningmodels, min_err)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

foodsec_cart.prn <- prune(
  foodsec_cart,
  cp = foodsec_cart$cptable[foodsec_cart$cptable[ ,2] == 19, 'CP']
  )

# rpart.plot(foodsec_cart.prn$finalmodel, yesno = T)
print(foodsec_cart.prn)

## Evaluate the model
## Measure training predictions against testing data set

# Variable Importance Plot
foodsec_cart.prn$variable.importance %>%
  data.frame() %>%
  rownames_to_column(var = 'Feature') %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = 'cadetblue', size = 0.3) +
  theme_minimal() +
  coord_flip() +
  labs(x = '', y = '', title = 'Variable Importance from Single Classification Tree')

# Note: House Income is a very heavy variable, will be 'greedily' selected by the model
# A random forest model will explore the value of other variables

# Confusion Matrix
foodsec.preds <- bind_cols(
  predict(foodsec_cart.prn, newdata = foodsec_test, type = 'prob'),
  Predicted = predict(foodsec_cart.prn, newdata = foodsec_test, type = 'vector'),
  Actual = foodsec_test$FoodSec
)

foodsec.preds$Predicted <- foodsec.preds$Predicted %>%
  as.factor() %>% 
  fct_collapse(FoodSecure = '1', FoodInsecure = '2')
glimpse(foodsec.preds)

foodsec.cm <- confusionMatrix(foodsec.preds$Predicted, foodsec.preds$Actual)
foodsec.cm

### Random Forest Model

## Define the training control parameters
# 10-fold cross validation using grid search

trctrl <- trainControl(
  method = 'cv',
  number = 10,
  search = 'grid'
)

## Train a basic model on the data using the default parameters
set.seed(42)
rf.foodsec.default <- train(
  FoodSec ~ .,
  data = foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  trControl = trctrl
)

print(rf.foodsec.default)

# mtry = 2
# Accuracy = .71

### Revise the basic model
## Find the optimal mtry
# mtry is the number of variables considered in a split

set.seed(42)
tunegrid <- expand.grid(.mtry = c(1:10))

rf.foodsec.mtry <- train(
  FoodSec ~ .,
  data = foodsec_train,
  method = 'rf',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  ntree = 500
)

print(rf.foodsec.mtry)

best_mtry <- rf.foodsec.mtry$bestTune$mtry
best_mtry

## Find the optimal number of nodes

store_maxnode <- list()

tunegrid <- expand.grid(.mtry = best_mtry) # Note: best_mtry is incorporated into the model via the tuning grid

for (maxnodes in c(3:30)) {
  set.seed(42)
  rf_maxnode <- train(
    FoodSec ~ .,
    data = foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    tuneGrid = tunegrid,
    trControl = trctrl,
    importance = T,
    nodesize = 14,
    maxnodes = maxnodes,
    ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}

results.bestmtry <- resamples(store_maxnode)
summary(results.bestmtry)

# Optimal number of max nodes is 25

## Find the optimal number of trees

store_maxtrees <-list()

for (ntree in c(250,300,450,500,550,600,700,800,1000,2000)) {
  set.seed(42)
  rf_maxtrees <- train(
    FoodSec ~ .,
    data = foodsec_train,
    method = 'rf',
    metric = 'Accuracy',
    trControl = trctrl,
    tuneGrid = tunegrid,
    importance = T,
    maxnodes = 25,
    ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}

results_tree <- resamples(store_maxtrees)
summary(results_tree)

## Optimal number of trees is 300

### Build the final model based on the tuning parameters

rf.foodsec.fit <- train(
  FoodSec ~ .,
  data = foodsec_train,
  method = 'rf',
  metric = 'Accuracy',
  tuneGrid = tunegrid,
  trControl = trctrl,
  importance = T,
  ntree = 300,
  maxnodes = 25
)

### Evaluate the model

predictions <- predict(
  rf.foodsec.fit,
  foodsec_test
)

foodsec.cm <- confusionMatrix(
  predictions,
  foodsec_test$FoodSec
)
print(foodsec.cm)

plot(varImp(rf.foodsec.fit))

rf.foodsec.fit %>% varImpPlot()
print(rf.foodsec.fit)
























