# Model for predicting wine quality from chemical composition
# Data set available on https://archive-beta.ics.uci.edu/ml/datasets/wine+quality

#libraries
#Note: train() function may prompt for installation of additional packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(pastecs)) install.packages("pastecs", repos = "http://cran.us.r-project.org")
if(!require(robustHD)) install.packages("robustHD", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(regclass)) install.packages("regclass", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(ordinalForest)) install.packages("ordinalForest", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

#elevate dplyr for common function conflicts in packages
select <- dplyr::select
filter <- dplyr::filter

#options
options(scipen=999) #avoid scientific notation
#options(digits = 3)

#Data exploration
# three files provided: winequality.names, winequality-white.csv, winequality-red.csv

#preview files to confirm formats and setup creation of dataframes
writeLines(readLines("./data/winequality.names", 20)) #citation notes
writeLines(readLines("./data/winequality-white.csv", 2))
#actually ";" delimited files with headers so we can read into dataframes directly
white_wines <- read_delim("./data/winequality-white.csv", delim = ";")
problems(white_wines) #no rows
str(white_wines)
red_wines <- read_delim("./data/winequality-red.csv", delim = ";")
problems(red_wines) #no rows
str(red_wines)

#convert names to remove spaces and replace with "_" which allows for easier unquoted reference
white_wines <- white_wines %>% rename_with(.fn = ~ str_replace_all(colnames(white_wines),"\\s+","_"))
red_wines <- red_wines %>% rename_with(.fn = ~ str_replace_all(colnames(red_wines),"\\s+","_"))
str(white_wines)

#Data cleansing

#calculate summary statistics
white_stats <- stat.desc(white_wines)
red_stats <- stat.desc(red_wines)

#check for na and null values.
sum(white_stats$nbr.na) + sum(white_stats$nbr.null) #0
sum(red_stats$nbr.na) + sum(red_stats$nbr.null) #0

#check distribution of ratings in each set
unique(white_wines$quality) %>% sort() #integers from 3 to 9
white_wines %>% ggplot(aes(x=quality)) + geom_histogram(binwidth = 1)
summary(white_wines$quality)
sd(white_wines$quality)

unique(red_wines$quality) %>% sort() #integers from 3 to 8
red_wines %>% ggplot(aes(x=quality)) + geom_histogram(binwidth = 1)
summary(red_wines$quality)
sd(red_wines$quality)

#Note both are approximately normal but a qq plot will yield better insight
#compare to normal curve with same mean/sd
w_params <- white_wines %>% summarize(mean = mean(quality), sd = sd(quality))
white_wines %>% ggplot(aes(sample=quality)) + geom_qq(dparams = w_params) + geom_abline()
#If quality ratings normal, we'd see an even spread of theoretical points either side of line
#However, there are more very low (3) and very high (9) ratings than expected for normal distribution

r_params <- red_wines %>% summarize(mean = mean(quality), sd = sd(quality))
red_wines %>% ggplot(aes(sample=quality)) + geom_qq(dparams = r_params) + geom_abline()

#Tabulation of ratings
white_wines %>% group_by(quality) %>% summarize(n=n())
red_wines %>% group_by(quality) %>% summarize(n=n())


#Visually explore relationship of ratings to independent variables, try facet for simultaneous
#The approach is to pivot longer so the charts are differentiated by the column with variable name
#https://stackoverflow.com/questions/44532634/several-plots-scatter-plot-of-a-specific-variable-vs-other-variables-in-r
#across applies an operation on set of columns
#https://dplyr.tidyverse.org/articles/colwise.html
#Note there were a few forms of using across() here but this seemed most direct and preserved col names
#https://statisticsglobe.com/standardize-data-frame-columns-in-r-scale-function
#We are standarizing independent variables only for presentation with a common y-axis in the facet
#white_wines %>% mutate(across(-c("quality"), ~ scale(.) %>% as.vector)) %>% 
#  pivot_longer(cols = -c("quality"), names_to = "yvar", values_to = "yval") %>% 
#  ggplot(aes(factor(quality), yval)) + geom_boxplot() + facet_grid(yvar ~ .)
#too difficult to interpret so just explore each variable separately !

#Scale independent variables
#Decided not to winsorize as outliers may have unique explanatory power in certain cases
#white_wines <- white_wines %>% mutate(across(-c("quality"), ~ winsorize(.) %>% as.vector))

white_wines <- white_wines %>% mutate(across(-c("quality"), ~ scale(.) %>% as.vector))
head(white_wines)
names(white_wines)
#"fixed_acidity"        "volatile_acidity"     "citric_acid"          "residual_sugar"       "chlorides"            "free_sulfur_dioxide" 
#"total_sulfur_dioxide" "density"              "pH"                   "sulphates"            "alcohol"              "quality"   
white_wines %>% ggplot(aes(x=factor(quality), y = fixed_acidity)) + geom_boxplot() #inverse trend with quality
white_wines %>% ggplot(aes(x=factor(quality), y = volatile_acidity)) + geom_boxplot() #higher values with lowest quality
white_wines %>% ggplot(aes(x=factor(quality), y = citric_acid)) + geom_boxplot()# no real trend
white_wines %>% ggplot(aes(x=factor(quality), y = residual_sugar)) + geom_boxplot()# bumps up for the "average 5/6 quality wines
white_wines %>% ggplot(aes(x=factor(quality), y = chlorides)) + geom_boxplot()#inverse trend with quality
white_wines %>% ggplot(aes(x=factor(quality), y = free_sulfur_dioxide)) + geom_boxplot()#lowest median for the quality 4 wines
white_wines %>% ggplot(aes(x=factor(quality), y = total_sulfur_dioxide)) + geom_boxplot()#Aside from q=4, inverse trend with qual
white_wines %>% ggplot(aes(x=factor(quality), y = density)) + geom_boxplot()#highest quality (7-9) have lowest density
white_wines %>% ggplot(aes(x=factor(quality), y = pH)) + geom_boxplot()#trend with quality except for worst (q=3)
white_wines %>% ggplot(aes(x=factor(quality), y = sulphates)) + geom_boxplot()# no clear trend
white_wines %>% ggplot(aes(x=factor(quality), y = alcohol)) + geom_boxplot()# generally positive trend, aside from q=5

#Look at correlation of independent variables and VIF to detect multicollinearity
#https://quantifyinghealth.com/correlation-collinearity-multicollinearity/
# plot the correlation matrix
corrplot(cor(white_wines %>% select(-"quality")), diag = FALSE, type="upper",
         title = "White Wine Feature Correlations", mar=c(0,0,2,0))
# then take a look at the VIF of each predictor
VIF(lm(quality ~ ., data=white_wines))
#since we expect density is ~ f(alcohol, sugar) the high VIF is somewhat expected 
#https://www.statology.org/multicollinearity-regression/
#Since our goal is prediction, rather than relationships we don't need to remove any features
#Though note the impact if density were removed
VIF(lm(quality ~ ., data=white_wines %>% select(-"density")))
#much better!

#Same steps for red wines here
red_wines <- red_wines %>% mutate(across(-c("quality"), ~ scale(.) %>% as.vector))
red_wines %>% ggplot(aes(x=factor(quality), y = fixed_acidity)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = volatile_acidity)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = citric_acid)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = residual_sugar)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = chlorides)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = free_sulfur_dioxide)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = total_sulfur_dioxide)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = density)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = pH)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = sulphates)) + geom_boxplot()
red_wines %>% ggplot(aes(x=factor(quality), y = alcohol)) + geom_boxplot()

# plot the correlation matrix
corrplot(cor(red_wines %>% select(-"quality")), diag = FALSE, type="upper",
         title = "Red Wine Feature Correlations", mar=c(0,0,2,0))
# then take a look at the VIF of each predictor
VIF(lm(quality ~ ., data=red_wines))
# multi-collinearity not as bad as with white wine

# Model criteria
# There are some tradeoffs in selecting a metric for "best" model here.
# Given the bell curve of ratings, we immediately have a prevalence issue where out MOST IMPORTANT ratings
# e.g very good and very bad are under-represented relative to "average" wines
# The author initially looked at modeling the full range of ratings and found poor results at the extremes
# This analysis collapses the ratings to a smaller set we're interested in from a decision perspective

# 0 (Bad): Ratings 3 & 4
# 1 (Ok): Ratings 5 & 6
# 2 (Good): Ratings 7-9
white_wines <- white_wines %>% 
  mutate(quality = ifelse(quality < 6, 0, ifelse(quality > 6, 2 , 1)))
red_wines <- red_wines %>% 
  mutate(quality = ifelse(quality < 6, 0, ifelse(quality > 6, 2 , 1)))


# Partition data 80/20 into train/test sets. Test is used to evaluate model options, but not used in model dev
# The selection of 20% for the test set is is based on usage with similar problems in the course materials
# Here this allows for ~proportional representation of classes in the train/test sets too!
y_white <- white_wines$quality
y_red <- red_wines$quality
#partition to test and train sets
set.seed(755, sample.kind = "Rounding") #R version > 3.6
#with a factor, the proportion of quality values will be preserved across the two sets
w_test_index <- createDataPartition(factor(y_white), p = 0.2, times = 1, list = FALSE)
w_train <- white_wines[-w_test_index,]
w_test <- white_wines[w_test_index,]
w_train %>% group_by(quality) %>% summarize(n=n() / nrow(w_train))
w_test %>% group_by(quality) %>% summarize(n=n() / nrow(w_test))
#confirmed that proportions of each class align between train/test
r_test_index <- createDataPartition(factor(y_red), p = 0.2, times = 1, list = FALSE)
r_train <- red_wines[-r_test_index,]
r_test <- red_wines[r_test_index,]

#Look at some regression/mixed model performance before switching to quality as a factor
reg_models <- c("lm", "knn", "gamLoess", "rf")
w_reg_fits <- lapply(reg_models, function(model){ 
  train(quality ~ ., method = model, data = w_train)
})

r_reg_fits <- lapply(reg_models, function(model){ 
  train(quality ~ ., method = model, data = r_train)
})
names(w_reg_fits) <- reg_models
names(r_reg_fits) <- reg_models

# Record Accuracy and class sensitivity and specificity for each of these models on the test set
#Create a shell tibble to populate with model results
w_model_summary <- tibble(model = "none", acc = 0, c0_sens = 0, c0_spec = 0,  c0_f1 = 0,
                          c1_sens = 0, c1_spec = 0, c1_f1 = 0, c2_sens = 0, c2_spec = 0, c2_f1 = 0)
r_model_summary <- copy(w_model_summary)
#Define levels of classification for confusion matrix
q_levels <- factor(c("0","1","2"),ordered = T)

#create a reusable function to process model fits with test data.
#Default assumes classification data and always requires alignment between model and test data on this
parse_fits <- function(fits, test_set, m_summary, m_class = TRUE) {
  for (i in 1:length(fits)) {
    m_predicted <-predict(fits[[i]], test_set)
    y_test <- test_set$quality
    m_suff <- "_cls"
    #coerce to factors if regression results
    if (m_class != TRUE) {
      m_predicted <- factor(round(m_predicted, 0), levels = q_levels)
      y_test <- factor(round(y_test, 0), levels = q_levels)
      m_suff <- "_reg"
    }
    #generate the confusion matrix with predicted values
    m_cm <-confusionMatrix(m_predicted, y_test)
    #add new row to model summary
    m_summary <-m_summary %>% 
      rbind(tibble(model = str_c(fits[[i]]$method, m_suff),
          acc = m_cm$overall["Accuracy"],
          c0_sens = m_cm$byClass[1, 1],
          c0_spec = m_cm$byClass[1, 2],
          c0_f1 = m_cm$byClass[1, 7],
          c1_sens = m_cm$byClass[2, 1],
          c1_spec = m_cm$byClass[2, 2],
          c1_f1 = m_cm$byClass[2, 7],
          c2_sens = m_cm$byClass[3, 1],
          c2_spec = m_cm$byClass[3, 2],
          c2_f1 = m_cm$byClass[3, 7]
        )
      )
  }
  return(m_summary)
}

w_model_summary <- parse_fits(w_reg_fits, w_test, w_model_summary, m_class = FALSE) %>%
  filter(acc > 0)
r_model_summary <- parse_fits(r_reg_fits, r_test, r_model_summary, m_class = FALSE) %>%
  filter(acc > 0)

#Prepare the data set for classification model training
white_wines <- white_wines %>% mutate(quality = factor(quality, levels = q_levels))
w_train <- w_train %>% mutate(quality = factor(quality, levels = q_levels))
w_test <- w_test %>% mutate(quality = factor(quality, levels = q_levels))
red_wines <- red_wines %>% mutate(quality = factor(quality, levels = q_levels))
r_train <- r_train %>% mutate(quality = factor(quality, levels = q_levels))
r_test <- r_test %>% mutate(quality = factor(quality, levels = q_levels))

#rf is one of many random forest libraries. Note that we have repeats that will now work with discreet classes
#we took the step to order the quality factor and try some models specifically geared to ordered factor variables

class_models <- c("naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf", "polr")
w_class_fits <- lapply(class_models, function(model){ 
  train(quality ~ ., method = model, data = w_train)
})
r_class_fits <- lapply(class_models, function(model){ 
  train(quality ~ ., method = model, data = r_train)
})
names(w_class_fits) <- class_models
names(r_class_fits) <- class_models

#parse results into respective summary tables
w_model_summary <- parse_fits(w_class_fits, w_test, w_model_summary) 
r_model_summary <- parse_fits(r_class_fits, r_test, r_model_summary)

#caret implementation of xgbTree had many warnings so chose to use package directly
# https://www.statology.org/xgboost-in-r/
# Note classification labels need to be numeric as 0 -> N-1
w_train_x <- w_train %>% select(-quality) %>% data.matrix()
w_train_y <- as.numeric(as.character(w_train$quality))
w_test_x <- w_test %>% select(-quality) %>% data.matrix()
w_test_y <- as.numeric(as.character(w_test$quality))
w_xgb_train <- xgb.DMatrix(data = w_train_x, label = w_train_y)
w_xgb_test <- xgb.DMatrix(data = w_test_x, label = w_test_y)

r_train_x <- r_train %>% select(-quality) %>% data.matrix()
r_train_y <- as.numeric(as.character(r_train$quality))
r_test_x <- r_test %>% select(-quality) %>% data.matrix()
r_test_y <- as.numeric(as.character(r_test$quality))
r_xgb_train <- xgb.DMatrix(data = r_train_x, label = r_train_y)
r_xgb_test <- xgb.DMatrix(data = r_test_x, label = r_test_y)

#define watchlists
w_watch <- list(train=w_xgb_train, test=w_xgb_test)
r_watch <- list(train=r_xgb_train, test=r_xgb_test)

#fit XGBoost model and display training and testing data at each round
#https://xgboost.readthedocs.io/en/stable/R-package/xgboostPresentation.html
xgb_params = list(booster = "gbtree",
               objective = "multi:softmax",
               num_class = 3)
w_xgb_fit <- xgb.train(data = w_xgb_train, max.depth = 3, watchlist=w_watch, params = xgb_params, nrounds = 70)
r_xgb_fit <- xgb.train(data = r_xgb_train, max.depth = 3, watchlist=r_watch, params = xgb_params, nrounds = 70)
#Note running additional rounds doesn't improve test mlogloss ~1 metric

#look at the rmse and accuracy next with predict() which yields numeric class prediction
str(predict(w_xgb_fit, newdata = w_xgb_test)) #"num"
unique(predict(w_xgb_fit, newdata = w_xgb_test)) # 1 0 2
#generate same measures as with other models trained in caret
w_xgb_cm <- confusionMatrix(factor(predict(w_xgb_fit, w_xgb_test), levels = q_levels), w_test$quality)
w_model_summary <- w_model_summary %>% rbind(tibble(model = "xgbtree_cls",
                    acc = w_xgb_cm$overall["Accuracy"], c0_sens = w_xgb_cm$byClass[1, 1],
                    c0_spec = w_xgb_cm$byClass[1, 2], c0_f1 = w_xgb_cm$byClass[1, 7],
                    c1_sens = w_xgb_cm$byClass[2, 1], c1_spec = w_xgb_cm$byClass[2, 2],
                    c1_f1 = w_xgb_cm$byClass[2, 7], c2_sens = w_xgb_cm$byClass[3, 1],
                    c2_spec = w_xgb_cm$byClass[3, 2], c2_f1 = w_xgb_cm$byClass[3, 7]))

r_xgb_cm <- confusionMatrix(factor(predict(r_xgb_fit, r_xgb_test), levels = q_levels), r_test$quality)
r_model_summary <- r_model_summary %>% rbind(tibble(model = "xgbtree_cls",
                    acc = w_xgb_cm$overall["Accuracy"], c0_sens = w_xgb_cm$byClass[1, 1],
                    c0_spec = w_xgb_cm$byClass[1, 2], c0_f1 = w_xgb_cm$byClass[1, 7],
                    c1_sens = w_xgb_cm$byClass[2, 1], c1_spec = w_xgb_cm$byClass[2, 2],
                    c1_f1 = w_xgb_cm$byClass[2, 7], c2_sens = w_xgb_cm$byClass[3, 1],
                    c2_spec = w_xgb_cm$byClass[3, 2], c2_f1 = w_xgb_cm$byClass[3, 7]))

# Results
# Though not the only metric of interest, the sensitivity on class zero is quite important
# Specifically we want to classify bad wines as bad. If some better wines get classified
# as bad that's acceptable in this context. (though unfortunate for the wine producer)

#WHITE WINES
w_model_summary %>% arrange(c0_sens)
# # A tibble: 11 × 11
# model             acc c0_sens c0_spec c0_f1 c1_sens c1_spec c1_f1 c2_sens c2_spec  c2_f1
# <chr>           <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl>  <dbl>
# 1 lm_reg          0.551   0.375   0.922 0.490   0.856   0.315 0.635   0.189   0.975  0.295
# 2 gamLoess_reg    0.567   0.473   0.905 0.569   0.797   0.387 0.625   0.236   0.960  0.341
# 3 knn_reg         0.593   0.558   0.857 0.606   0.691   0.526 0.608   0.443   0.935  0.528
# 4 multinom_cls    0.588   0.607   0.839 0.630   0.691   0.544 0.614   0.344   0.931  0.432
# 5 gamLoess_cls    0.559   0.616   0.854 0.646   0.786   0.376 0.616   0       1     NA    
# 6 svmLinear_cls   0.559   0.619   0.831 0.633   0.784   0.404 0.623   0       1     NA    
# 7 knn_cls         0.595   0.628   0.817 0.631   0.591   0.644 0.583   0.552   0.888  0.564
# 8 rf_reg          0.701   0.640   0.928 0.718   0.855   0.578 0.720   0.476   0.977  0.610
# 9 naive_bayes_cls 0.563   0.677   0.771 0.635   0.473   0.748 0.531   0.575   0.814  0.512
# 10 polr_cls        0.574   0.683   0.768 0.637   0.675   0.55  0.606   0.198   0.970  0.303
# 11 rf_cls          0.740   0.744   0.883 0.753   0.780   0.720 0.734   0.651   0.964  0.730

#Random forest classification seems to do the best in general, though some other models
#outperform on specific measures.
#Looking further, we can see the exact nature of predictions in a confusion matrix
confusionMatrix(predict(w_class_fits[["rf"]],w_test), w_test$quality)[["table"]]
#               Reference
# Prediction   0   1   2
#           0 244  73   3
#           1  80 343  71
#           2   4  24 138

#Check which features were most important for prediction
varImp(w_class_fits[["rf"]])
# Overall
# alcohol               100.00
# density                89.34
# volatile_acidity       55.08
# free_sulfur_dioxide    43.50
# total_sulfur_dioxide   33.94
# chlorides              31.54
# residual_sugar         31.53
# pH                     20.16
# citric_acid            15.34
# sulphates               3.56
# fixed_acidity           0.00

#RED WINES
r_model_summary %>% arrange(c0_sens)
# # A tibble: 12 × 11
# model             acc c0_sens c0_spec c0_f1 c1_sens c1_spec c1_f1 c2_sens c2_spec  c2_f1
# <chr>           <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl>  <dbl>
# 1 gamLoess_reg    0.611   0.671   0.785 0.699   0.680   0.565 0.582   0.205   0.986  0.316
# 2 xgbtree_cls     0.655   0.683   0.862 0.698   0.702   0.652 0.660   0.514   0.922  0.572
# 3 knn_reg         0.620   0.691   0.779 0.710   0.641   0.622 0.580   0.318   0.960  0.406
# 4 lm_reg          0.648   0.725   0.802 0.742   0.734   0.596 0.627   0.136   0.996  0.235
# 5 knn_cls         0.604   0.732   0.709 0.708   0.531   0.689 0.531   0.386   0.939  0.436
# 6 rf_reg          0.717   0.738   0.866 0.780   0.812   0.653 0.696   0.364   0.996  0.525
# 7 gamLoess_cls    0.607   0.745   0.744 0.730   0.656   0.575 0.571   0       1     NA    
# 8 naive_bayes_cls 0.620   0.799   0.686 0.739   0.422   0.793 0.486   0.591   0.899  0.531
# 9 multinom_cls    0.654   0.826   0.715 0.766   0.562   0.725 0.569   0.341   0.968  0.441
# 10 rf_cls          0.729   0.826   0.814 0.809   0.672   0.767 0.664   0.568   0.964  0.633
# 11 polr_cls        0.648   0.826   0.709 0.764   0.578   0.699 0.569   0.25    0.982  0.367
# 12 svmLinear_cls   0.632   0.839   0.692 0.765   0.547   0.699 0.547   0.182   0.975  0.271

# Although polr and svmLinear are slightly better on c0 sensitivity, the random forest classification
# model is much better on overall accuracy and across the other balanced class measures
confusionMatrix(predict(r_class_fits[["rf"]],r_test), r_test$quality)[["table"]]
#               Reference
# Prediction   0   1   2
#           0 123  32   0
#           1  26  86  19
#           2   0  10  25

#Check which features were most important for prediction
varImp(r_class_fits[["rf"]])
# Overall
# alcohol               100.00
# sulphates              65.85
# volatile_acidity       51.85
# total_sulfur_dioxide   51.54
# density                36.74
# chlorides              20.40
# citric_acid            19.03
# fixed_acidity          11.30
# pH                      9.42
# residual_sugar          3.07
# free_sulfur_dioxide     0.00

#save(list=c("w_model_summary","r_model_summary","w_reg_fits","r_reg_fits","w_class_fits","r_class_fits"), file = "model_results.R")
