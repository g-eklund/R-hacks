rm(list=ls(all=TRUE)) 
dataset <- read.csv2("C:/Users/gustav.eklund/Data_Science/R/Hist_emp2.txt", sep = "\t" )
dataset$Estimated.Income<-  as.numeric(gsub(",",".",gsub(" ","",dataset$Estimated.Income)))
dataset$Survey.Year<-  as.numeric(gsub(" ","",dataset$Survey.Year))
dataset$Survey.Date <- as.Date(dataset$Survey.Date)
dataset$Survey.Date <- NULL

library(dummies)
Training <- dummy(dataset$Training, sep = "_")
Benefit <- dummy(dataset$Benefit, sep = "_")
Facility <- dummy(dataset$Facility, sep = "_")
Position2 <- dummy(dataset$Position, sep = "_")
Degree <- dummy(dataset$Degree, sep = "_")
Enjoy <- dummy(dataset$Enjoyable, sep = "_")
Challenging <- dummy(dataset$Challenging, sep = "_")

Main.onehot <- as.data.frame(cbind(dataset$ID,
              Training,
              Benefit,
              Facility,
              Position2,
              Degree,
              Enjoy,
              Challenging,
              Income = dataset$Estimated.Income,
              LengthOfHire = dataset$Length.of.Hire,
              DriveTime = dataset$Drive.Time.Code,
              Churn = dataset$Employee.Status,
              Hours = dataset$Hours.Worked,
              Year = dataset$Survey.Year))

Label <- Main.onehot$Churn
Main.onehot$Churn <- NULL
colnames <- colnames(Main.onehot)
colnames(Main.onehot) <- paste(rep("X",ncol(Main.onehot)),1:ncol(Main.onehot),sep = "")
colnames <- cbind(colnames,colnames(Main.onehot))
Main.onehot <- data.frame(Main.onehot, Y =Label)
Main.onehot$Y[Main.onehot$Y == 1] = 0
Main.onehot$Y[Main.onehot$Y == 2] = 1
Main.onehot$Y <- as.factor(Main.onehot$Y)

#__________Preprocessing done, setting up model


library(caret)
library(xgboost)

set.seed(500)
training.ind <- sample(1:nrow(Main.onehot),0.7*nrow(Main.onehot), replace = FALSE)
test.data <- Main.onehot[-training.ind,]
training.data <- Main.onehot[training.ind,]

testgrid <- expand.grid(
  nrounds = 10, 
  max_depth = 6,
  eta = c(0.3),
  gamma = c(0,1),
  colsample_bytree = 0.8,
  min_child_weight = 1, 
  subsample =0.5)

trcontrol <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = T,
  returnData = F,
  returnResamp = "all",
  classProbs = F,
  summaryFunction = twoClassSummary)


# Build formula
formula.test <- "Y~"
cname <- colnames(training.data)
for (i in 1:28){
  formula.test <- paste(formula.test,cname[i],"+",sep = "")
  if (i== length(cname)-1){
    formula.test <- paste(formula.test,cname[i],sep = "")
  }
}

form <- as.formula(formula.test)

xgT.model <- train(form, data = training.data,
                   trControl = trcontrol,
                   tuneGrid = testgrid,
                   method = "xgbTree")

