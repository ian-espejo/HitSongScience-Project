# HarvardX
# Professional Certificate in Data Science
# PH125.9x - Capstone: Hit Song Science Project
# Ian Espejo Campos

#_____________________________________________________________________________________
# /// WARNING: k-Nearest Neighbors and Random Forest's sections might take a while ///
#-------------------------------------------------------------------------------------

# 1.0 DATA EXTRACTION

# 1.1 Import libraries
rm(list=ls())

if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(broom))
  install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(randomForest))
  install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(broom)
library(knitr)
library(randomForest)

# 1.2 Import CSV file from Documents
songs_dataset <- read.csv("~/songAttributes_1999-2019.csv", header = T)

# 2.0 DATA ANALYSIS

# 2.1 Descriptive statistics
dim(songs_dataset)
## [1] 154931 18

head(songs_dataset) %>% as.tibble()
# A tibble: 6 x 18
# X Acousticness Album Artist Danceability Duration Energy Explicit
# <int>        <dbl> <chr> <chr>         <dbl>    <int>  <dbl> <chr>   
#   1     0    0.000728  Coll~ Colle~        0.52    234947  0.904 False   
# 2     1    0.0182    Coll~ Colle~        0.581   239573  0.709 False   
# 3     2    0.000473  Coll~ Colle~        0.572   198400  0.918 False   
# 4     3    0.00097   Coll~ Colle~        0.596   231453  0.661 False   
# 5     4    0.0000358 Coll~ Colle~        0.52    222520  0.808 False   
# 6     5    0.0106    Coll~ Colle~        0.353   263880  0.754 False   
# ... with 10 more variables: Instrumentalness <dbl>, Liveness <dbl>,
#   Loudness <dbl>, Mode <int>, Name <chr>, Popularity <int>,
#   Speechiness <dbl>, Tempo <dbl>, TimeSignature <int>, Valence <dbl>

summary(songs_dataset)
# X         Acousticness       Album              Artist           Danceability   
# Min.   :   0   Min.   :0.0000   Length:154931      Length:154931      Min.   :0.0000  
# 1st Qu.:  39   1st Qu.:0.0202   Class :character   Class :character   1st Qu.:0.4680  
# Median :  91   Median :0.1280   Mode  :character   Mode  :character   Median :0.5860  
# Mean   : 169   Mean   :0.2663                                         Mean   :0.5755  
# 3rd Qu.: 197   3rd Qu.:0.4530                                         3rd Qu.:0.6970  
# Max.   :1796   Max.   :0.9960                                         Max.   :0.9870  
# Duration           Energy         Explicit         Instrumentalness       Liveness     
# Min.   :   1731   Min.   :0.0000   Length:154931      Min.   :0.0000000   Min.   :0.0000  
# 1st Qu.: 188907   1st Qu.:0.4830   Class :character   1st Qu.:0.0000000   1st Qu.:0.1020  
# Median : 224747   Median :0.6780   Mode  :character   Median :0.0000033   Median :0.1540  
# Mean   : 232445   Mean   :0.6387                      Mean   :0.0630116   Mean   :0.2543  
# 3rd Qu.: 263646   3rd Qu.:0.8290                      3rd Qu.:0.0008690   3rd Qu.:0.3260  
# Max.   :4795973   Max.   :1.0000                      Max.   :0.9980000   Max.   :1.0000  
# Loudness            Mode           Name             Popularity     Speechiness    
# Min.   :-60.000   Min.   :0.000   Length:154931      Min.   : 0.00   Min.   :0.0000  
# 1st Qu.: -9.852   1st Qu.:0.000   Class :character   1st Qu.: 6.00   1st Qu.:0.0349  
# Median : -6.992   Median :1.000   Mode  :character   Median :17.00   Median :0.0528  
# Mean   : -8.027   Mean   :0.683                      Mean   :20.25   Mean   :0.1224  
# 3rd Qu.: -5.166   3rd Qu.:1.000                      3rd Qu.:31.00   3rd Qu.:0.1450  
# Max.   :  3.515   Max.   :1.000                      Max.   :91.00   Max.   :0.9690  
# Tempo        TimeSignature      Valence      
# Min.   :  0.00   Min.   :0.000   Min.   :0.0000  
# 1st Qu.: 94.37   1st Qu.:4.000   1st Qu.:0.3090  
# Median :118.86   Median :4.000   Median :0.4980  
# Mean   :119.22   Mean   :3.904   Mean   :0.4985  
# 3rd Qu.:139.82   3rd Qu.:4.000   3rd Qu.:0.6880  
# Max.   :248.06   Max.   :5.000   Max.   :0.9950  

summarize(songs_dataset,
          unique_artist = n_distinct(Artist),
          unique_albums = n_distinct(Album))
## unique_artist unique_albums
## 1 989 9799

songs_dataset %>%
  arrange(desc(Popularity)) %>%
  slice(1:20) %>%
  mutate(Ranking = 1:n()) %>%
  select(Ranking, Popularity, Name, Artist) %>%
  kable()
# | Ranking| Popularity|Name                                                 |Artist              |
# |-------:|----------:|:----------------------------------------------------|:-------------------|
# |       1|         91|Hot (feat. Gunna)                                    |Young Thug          |
# |       2|         91|Lover                                                |Taylor Swift        |
# |       3|         91|South of the Border (feat. Camila Cabello & Cardi B) |Ed Sheeran          |
# |       4|         90|No Guidance (feat. Drake)                            |Chris Brown         |
# |       5|         89|You Need To Calm Down                                |Taylor Swift        |
# |       6|         89|Beautiful People (feat. Khalid)                      |Ed Sheeran          |
# |       7|         88|Only Human                                           |Jonas Brothers      |
# |       8|         88|Perfect                                              |Ed Sheeran          |
# |       9|         87|Bad Bad Bad (feat. Lil Baby)                         |Young Thug          |
# |      10|         87|High Hopes                                           |Panic! At The Disco |
# |      11|         87|ME! (feat. Brendon Urie of Panic! At The Disco)      |Taylor Swift        |
# |      12|         87|Shape of You                                         |Ed Sheeran          |
# |      13|         87|I Don't Care (with Justin Bieber)                    |Ed Sheeran          |
# |      14|         86|EARFQUAKE                                            |Tyler The Creator   |
# |      15|         86|Going Bad (feat. Drake)                              |Meek Mill           |
# |      16|         86|The Man                                              |Taylor Swift        |
# |      17|         86|God's Plan                                           |Drake               |
# |      18|         85|I Forgot That You Existed                            |Taylor Swift        |
# |      19|         85|Cruel Summer                                         |Taylor Swift        |
# |      20|         85|Photograph                                           |Ed Sheeran          |

# 2.2 Variables selection
songs_dataset <- songs_dataset %>%
  select(Popularity, Danceability, Energy, Loudness, Speechiness, Acousticness,
         Instrumentalness, Liveness, Valence, Tempo, Duration)

# 2.3 Correlation between Popularity and features
cor(songs_dataset[ ,1],
    songs_dataset[ ,sapply(songs_dataset, is.numeric)])
##     Popularity Danceability     Energy  Loudness Speechiness Acousticness
## [1,]         1 -0.007350333 0.09923257 0.1469342  -0.1261396     -0.10745
##     Instrumentalness    Liveness     Valence Tempo     Duration
## [1,]     -0.06061138 -0.05928446 -0.02940333 0.0449777 0.04561194

# 2.4 Popularity properties
mean(songs_dataset$Popularity)
## [1] 20.24911

sd(songs_dataset$Popularity)
## [1] 16.50665

mean(songs_dataset$Popularity) + sd(songs_dataset$Popularity)
## [1] 36.75576

# 2.5 'Hit' flag defined as songs with Popularity greater or equal to 40
songs_dataset <- songs_dataset %>%
  mutate(Hit = as.factor(ifelse(Popularity < 40, 0, 1))) %>%
  select(-Popularity) %>%
  select(Hit, everything())

# 2.6 Index with 0.2 of probability
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(songs_dataset$Hit,
                                  times = 1,
                                  p = 0.2,
                                  list = F)

# 2.7 Train and Test sets creation
test_set <- songs_dataset[test_index,]
nrow(test_set)
## [1] 30987

train_set <- songs_dataset[-test_index,]
nrow(train_set)
## [1] 123944

mean(train_set$Hit == 1)
## [1] 0.1371264

# 3.0 DATA VISUALIZATION

# 3.1 Relation between Loudness and Energy against Hits
train_set %>% ggplot(aes(Loudness, Energy, color = Hit)) +
  geom_point() +
  theme_update() +
  labs(x = "Loudness", y = "Energy") +
  ggtitle("Figure 2.1") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

# 3.2 Relation between Danceability and Duration against Hits
train_set %>% ggplot(aes(Danceability, Duration, color = Hit)) +
  geom_point() +
  theme_update() +
  labs(x = "Danceability", y = "Duration") +
  ggtitle("Figure 2.2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

# 3.3 Relation between Speechiness and Instrumentalness against Hits
train_set %>% ggplot(aes(Speechiness, Instrumentalness, color = Hit)) +
  geom_point() +
  theme_update() +
  labs(x = "Speechiness", y = "Instrumentalness") +
  ggtitle("Figure 2.3") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

# 3.0 DATA MODELLING

# 3.1 Logistic Regressions
tidy(glm(Hit ~ Danceability, data = train_set, family = binomial))
# A tibble: 2 x 5
# term         estimate std.error statistic      p.value
# <chr>           <dbl>     <dbl>     <dbl>        <dbl>
# 1 (Intercept)    -2.00     0.0299    -66.7  0           
# 2 Danceability    0.271    0.0495      5.47 0.0000000450

tidy(glm(Hit ~ Energy, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
# 1 (Intercept)   -2.38     0.0263     -90.5 0.       
# 2 Energy         0.830    0.0373      22.3 9.89e-110

tidy(glm(Hit ~ Loudness, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
# 1 (Intercept)  -1.15     0.0195      -58.8 0.       
# 2 Loudness      0.0920   0.00251      36.7 3.19e-295

tidy(glm(Hit ~ Speechiness, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
# 1 (Intercept)    -1.67    0.0108    -155.  0.       
# 2 Speechiness    -1.51    0.0685     -22.1 4.15e-108

tidy(glm(Hit ~ Acousticness, data = train_set, family = binomial))
# A tibble: 2 x 5
# term         estimate std.error statistic   p.value
# <chr>           <dbl>     <dbl>     <dbl>     <dbl>
# 1 (Intercept)    -1.65     0.0106    -155.  0.       
# 2 Acousticness   -0.785    0.0305     -25.8 1.60e-146

tidy(glm(Hit ~ Instrumentalness, data = train_set, family = binomial))
# A tibble: 2 x 5
# term             estimate std.error statistic  p.value
# <chr>               <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)        -1.80    0.00856    -210.  0.      
# 2 Instrumentalness   -0.719   0.0500      -14.4 9.08e-47

tidy(glm(Hit ~ Liveness, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
# 1 (Intercept)    -1.59    0.0124    -128.  0.       
# 2 Liveness       -1.07    0.0425     -25.2 5.14e-140

tidy(glm(Hit ~ Valence, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic  p.value
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)   -1.73     0.0187    -92.2  0.      
# 2 Valence       -0.224    0.0344     -6.50 7.91e-11

tidy(glm(Hit ~ Tempo, data = train_set, family = binomial))
# A tibble: 2 x 5
# term        estimate std.error statistic  p.value
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) -2.16     0.0332       -65.2 0.      
# 2 Tempo        0.00267  0.000265      10.1 7.68e-24

tidy(glm(Hit ~ Duration, data = train_set, family = binomial))
# A tibble: 2 x 5
# term            estimate    std.error statistic  p.value
# <chr>              <dbl>        <dbl>     <dbl>    <dbl>
# 1 (Intercept) -1.93        0.0155         -125.   0.      
# 2 Duration     0.000000401 0.0000000552      7.25 4.02e-13

# 3.2 Logistic Model
log_model <- glm(Hit ~ ., data = train_set, family = binomial)

summary(log_model)
# Call:
#   glm(formula = Hit ~ ., family = binomial, data = train_set)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0251  -0.6046  -0.5098  -0.3882   3.3360  
# 
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -7.706e-01  9.219e-02  -8.359  < 2e-16 ***
# Danceability      2.543e-01  6.332e-02   4.017 5.90e-05 ***
# Energy           -1.596e-03  7.576e-02  -0.021  0.98319    
# Loudness          8.002e-02  4.067e-03  19.675  < 2e-16 ***
# Speechiness      -1.440e+00  7.862e-02 -18.319  < 2e-16 ***
# Acousticness     -3.078e-01  4.398e-02  -6.999 2.58e-12 ***
# Instrumentalness -7.306e-01  5.425e-02 -13.467  < 2e-16 ***
# Liveness         -9.954e-01  4.509e-02 -22.076  < 2e-16 ***
# Valence          -6.157e-01  4.402e-02 -13.987  < 2e-16 ***
# Tempo             1.202e-03  2.855e-04   4.208 2.57e-05 ***
# Duration          2.034e-07  6.574e-08   3.094  0.00198 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 99084  on 123943  degrees of freedom
# Residual deviance: 95896  on 123933  degrees of freedom
# AIC: 95918
# 
# Number of Fisher Scoring iterations: 5

tidy(log_model, conf.int = T)
# # A tibble: 11 x 7
# term                 estimate    std.error statistic   p.value      conf.low    conf.high
# <chr>                   <dbl>        <dbl>     <dbl>     <dbl>         <dbl>        <dbl>
# 1 (Intercept)       -0.771       0.0922         -8.36   6.35e- 17 -0.951        -0.590      
# 2 Danceability       0.254       0.0633          4.02   5.90e-  5  0.130         0.379      
# 3 Energy            -0.00160     0.0758         -0.0211 9.83e-  1 -0.150         0.147      
# 4 Loudness           0.0800      0.00407        19.7    3.52e- 86  0.0721        0.0880     
# 5 Speechiness       -1.44        0.0786        -18.3    5.83e- 75 -1.60         -1.29       
# 6 Acousticness      -0.308       0.0440         -7.00   2.58e- 12 -0.394        -0.222      
# 7 Instrumentalness  -0.731       0.0543        -13.5    2.45e- 41 -0.838        -0.625      
# 8 Liveness          -0.995       0.0451        -22.1    5.43e-108 -1.08         -0.907      
# 9 Valence           -0.616       0.0440        -14.0    1.87e- 44 -0.702        -0.529      
# 10 Tempo             0.00120     0.000286        4.21   2.57e-  5  0.000642      0.00176    
# 11 Duration          0.000000203 0.0000000657    3.09   1.98e-  3  0.0000000707  0.000000329

# 3.3 Guessing Model
p <- 0.9
set.seed(2, sample.kind = "Rounding")
guess <- sample(c(0,1),
                length(test_index),
                replace = T,
                prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$Hit))

mean(guess == test_set$Hit)
## [1] 0.7899442

# 3.4 k-Nearest Neighbor Model

# 3.4.1 10-fold cross validation (25 ks)
set.seed(3, sample.kind = "Rounding")
train_knn_cv <- train(Hit ~ .,
                      method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               p = .9))

ggplot(train_knn_cv, highlight = T)

confusionMatrix(predict(train_knn_cv, test_set, type = "raw"),
                test_set$Hit)$overall["Accuracy"]
## Accuracy
## 0.8676219

# 3.4.2  25-fold cross validation (5 ks)
set.seed(3, sample.kind = "Rounding")
train_knn <- train(Hit ~ .,
                   method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(27, 35, 2)))

ggplot(train_knn, highlight = T)

train_knn$finalModel
## 35-nearest neighbor model
## Training set outcome distribution:
##
##      0     1
## 106948 16996

confusionMatrix(predict(train_knn, test_set, type = "raw"),
                test_set$Hit)$overall["Accuracy"]
## Accuracy
## 0.8628134

# 3.5 Generalized Linear Model
set.seed(2, sample.kind = "Rounding")
train_glm <- train(Hit ~ .,
                   method = "glm",
                   data = train_set)

train_glm
## Generalized Linear Model
##
## 123944 samples
##     10 predictor
##      2 classes: '0', '1'
##
## No pre-processing
## Resampling: Bootstrapped (25 reps)
## Summary of sample sizes: 123944, 123944, 123944, 123944, 123944, 123944, ...
## Resampling results:
##
##  Accuracy        Kappa
## 0.8629126 -5.27819e-06

# 3.6 Linear Discriminant Analysis Model
set.seed(2, sample.kind = "Rounding")
train_lda <- train(Hit ~ .,
                   method = "lda",
                   data = train_set)

train_lda
## Linear Discriminant Analysis
##
## 123944 samples
##     10 predictor
##      2 classes: '0', '1'
##
## No pre-processing
## Resampling: Bootstrapped (25 reps)
## Summary of sample sizes: 123944, 123944, 123944, 123944, 123944, 123944, ...
## Resampling results:
##
##  Accuracy Kappa
## 0.8629152     0

# 3.7 Random Forest

# 3.7.1 100 trees (10 samples)
set.seed(5, sample.kind = "Rounding")
train_rf_100t <-  train(Hit ~ .,
                        data = train_set,
                        method = "rf", 
                        ntree = 100,
                        tuneGrid = data.frame(mtry = 1:10))

ggplot(train_rf_100t)

confusionMatrix(predict(train_rf_100t, test_set),
                test_set$Hit)$overall["Accuracy"]
## Accuracy
## 0.8628780

# 3.7.2 500 trees (5 samples)
set.seed(5, sample.kind = "Rounding")
train_rf <-  train(Hit ~ .,
                   data = train_set,
                   method = "rf",
                   tuneGrid = data.frame(mtry = 1:5))

ggplot(train_rf)

train_rf$finalModel
## Call:
## randomForest(x = x, y = y, mtry = param$mtry)
## Type of random forest: classification
## Number of trees: 500
## No. of variables tried at each split: 1
##
## OOB estimate of error rate: 13.21%
## Confusion matrix:
##        0    1 class.error
## 0 106264  684 0.006395632
## 1  15691 1305 0.923217228

varImp(train_rf)
## rf variable importance
##
##               Overall
## Duration       100.00
## Loudness        99.31
## Acousticness    88.61
## Tempo           88.12
## Liveness        85.15
## Speechiness     83.66
## Valence         79.62
## Energy          79.59
## Danceability    78.97
## Instrumentalness 0.00

rf_preds <- predict(train_rf, test_set)
cm <- confusionMatrix(rf_preds, test_set$Hit)
cm$overall["Accuracy"]
## Accuracy
## 0.8678801

cm
## Confusion Matrix and Statistics
##
## Reference
## Prediction     0    1
##          0 26553 3909
##          1   185  340
##
##               Accuracy : 0.8679
##                 95% CI : (0.8641, 0.8716)
##    No Information Rate : 0.8629
##    P-Value [Acc > NIR] : 0.005192
##
##                  Kappa : 0.1158
##
## Mcnemar's Test P-Value : < 2.2e-16
##
##            Sensitivity : 0.99308
##            Specificity : 0.08002
##         Pos Pred Value : 0.87168
##         Neg Pred Value : 0.64762
##             Prevalence : 0.86288
##         Detection Rate : 0.85691
##   Detection Prevalence : 0.98306
##      Balanced Accuracy : 0.53655
##
## 'Positive' Class : 0
