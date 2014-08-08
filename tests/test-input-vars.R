library(testthat)
library(predcomps)

# test get-input-vars method

library(caret)

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

class(knnFit1)

data(cars)
TrainData <- cars[, 1:4]
TrainClasses <- cars[, 5]
TrainClasses <- factor(TrainClasses)

knnFit2 <- train(TrainData, TrainClasses,
                 method = "glm",
                 tuneLength = 10, 
                 trControl = trainControl(method = "boot"))


apcDF <- GetPredCompsDF(knnFit2, cars)

credit <- read.csv("https://github.com/invinciblejha/Kaggle-1/raw/master/Give%20Me%20Some%20Credit/cs-training.csv")
#  We will transitions starting at each of 500 random rows
numForTransitionStart <- 500
#  ... going to each of 10,000 other random rows:
numForTransitionEnd <- 10000
#  ... keeping only the nearest 100 pairs for each start:
onlyIncludeNearestN = 100

# 100 trees for random forest 
ntree = 100

set.seed(1)
library(randomForest)
credit <- na.omit(credit)
# Turning the response to type "factor" causes the RF to be build for classification:
credit$SeriousDlqin2yrs <- factor(credit$SeriousDlqin2yrs) 
rfFit <- randomForest(SeriousDlqin2yrs ~ ., data=credit, ntree=ntree)


knnFit2 <- train(SeriousDlqin2yrs ~ ., data=credit,
                 method = "knn",
                 tuneLength = 5, 
                 trControl = trainControl(method = "cv", number = 3))


set.seed(1)
apcDF <- GetPredCompsDF(rfFit, credit,
                        numForTransitionStart = numForTransitionStart,
                        numForTransitionEnd = numForTransitionEnd,
                        onlyIncludeNearestN = onlyIncludeNearestN)

apcDF2 <- GetPredCompsDF(knnFit2, credit,
                        numForTransitionStart = numForTransitionStart,
                        numForTransitionEnd = numForTransitionEnd,
                        onlyIncludeNearestN = onlyIncludeNearestN)

############
#
N <- 200
vValues <- (-3):3
v <- sample(vValues, N, replace=TRUE)

df <- data.frame(v)
for (i in seq_along(vValues)) {
  df[[paste0("u",i)]] <- 
    ifelse(v==vValues[i], 
           sample(c(0,10), N, replace=TRUE),  # u can be either 0 or 10 at one v value
           rep(0, N)                          # u is always 0 at other ones
    )
}

# u8 can transition at either v=-3 or v=3:
df$u8 <- ifelse(v %in% c(-3,3),
                sample(c(0,10), N, replace=TRUE), 
                rep(0, N)                         
)

outcomeGenerationFunction <- function(df) {
  with(df, v*u1 + v*u2 + v*u3 + v*u4 + v*u5 + v*u6 + v*u7 + v*u8)
}
df$y <- outcomeGenerationFunction(df)


inputVars <- c("v",paste0("u",1:8))
apcDF <- GetPredCompsDF(outcomeGenerationFunction, df, inputVars = inputVars)