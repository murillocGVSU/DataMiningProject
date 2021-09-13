#CIS635_FinalProject
#Weekly Script
#By: Cleyde Murillo, Jr.

#On opening please minimize the function to easily see each step
#--------------------------------------------------------------------------------

#Loading 4 Data Sources
trainA <- read.csv('dataTrainA.txt', header = TRUE, sep = '\t')
trainB <- read.csv('dataTrainB.txt', header = TRUE, sep = '\t')
testA <- read.csv('dataTestA.txt', header = TRUE, sep = '\t')
testB <- read.csv('dataTestB.txt', header = TRUE, sep = '\t')

#Weekly Function
WeeklyAnalysis <- function(srcA, srcB, srcC, srcD){
  
  library(tidyverse)
  library(ggcorrplot)
  library(reshape2)
  library(gridExtra)
  library(e1071)
  library(rpart)
  library(lattice)
  library(caret)
  library(rpart.plot)
  library(ggfortify)
  library(ggthemes)
  
  train <- merge(srcA, srcB, by = 'id')
  train <- train[order(train$id), -13]
  
  test <- merge(srcC, srcD, by = 'id')
  test <- test[order(test$id), -13]
  
  outliers <- subset(train, 
                     (train$temp < 90) | (train$temp > 106) | (train$bpSys < 90) | (train$bpSys > 150) | (train$vo2 < 10) | 
                       (train$vo2 > 70) | (train$throat < 80) | (train$throat > 120) | (train$headA > 9) | (train$bodyA > 9))
  
  #Replaces NAs with medians
  train <- train %>% 
    mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
  
  #Remove outliers
  train <- anti_join(train, outliers)
  
  #Convert to Factors
  for(i in c(6:12)){
    train[,i] <- as.factor(train[,i])
    test[,i] <- as.factor(test[,i])
  }
  
  #Model + Tree Plot
  dt <- rpart(atRisk.x ~ . -id, data = train)
  pred <- predict(dt, test, type = 'class')
  dtx <- table(true = pred, predict = test$atRisk.x)
  confusionMatrix(dtx)
  qt <- cbind(test,pred)
  qt <- qt[,c(1:5,7:12,6,13)]
  
  qt <- qt %>%
    mutate(Status = if_else(atRisk.x == pred, 'Correct', 'Incorrect'))
  qt$Status <- as.factor(qt$Status)
  
  #Plots
  b1 <- ggplot(qt, aes(x = temp, y = headA, color = Status)) + 
    ggtitle('Predicted Virus Risk') +
    ylab('Head Ache Score') +
    xlab('Body Temperature') +
    geom_point() +
    scale_color_manual(values = c("#4699DD",'#C93232'))
  theme_economist()
  
  qtt <- confusionMatrix(pred, test$atRisk.x)
  ct <- qtt$table
  
  #Textfile, PNGs
  write.csv(qt, file = 'PredictedData.txt', col.names = TRUE, row.names = FALSE)
  
  png('Tree_Plot.png')
  rpart.plot(dt, extra = 104, nn = TRUE)
  ggsave('PredictedDataBarPlot.png', plot = b1)
  ggsave('FourFoldPlot_ConfusionMatrix.png', plot = fourfoldplot(ct, color = c("#CC6666", "#99CC99"),
                                                                 conf.level = 0, margin = 1, main = "Confusion Matrix"))
  dev.off()
  
}

#TESTING - Weekly Function - THIS WILL PRODUCE 3 PLOTS & 1 FILE (.TXT) WITH PREDICTIONS INTO YOUR DIRECTORY
WeeklyAnalysis(trainA, trainB, testA, testB)
