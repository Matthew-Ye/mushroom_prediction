# setwd to your local direction firstly
library(readr)
library(sqldf)
library(ggplot2)
library(caret)
library(randomForest)
library(caTools)   #<- For stratified split
library(rpart.plot)


mushrooms <- read.csv("mushrooms.csv",header=TRUE)
dim(mushrooms)
str(mushrooms)
head(mushrooms)
summary(mushrooms)
#tables
mush_features <- colnames(mushrooms)[-1]
tables <- lapply(mush_features, function(x) {table(mushrooms$class, mushrooms[,x])})
names(tables) <- mush_features
print(tables)
#correlation analysis
chisq_test_res = list()
for (i in 2:length(colnames(mushrooms))) {
  fname = colnames(mushrooms)[i]
  res = chisq.test(mushrooms[,i], mushrooms[,"class"], simulate.p.value = TRUE)
  res$data.name = paste(fname, "class", sep= " and ")
  chisq_test_res[[fname]] = res
}
chisq_test_res
#perform query on thedataset
query_1 <- sqldf("select class,population from mushrooms where habitat =='d'")
table(query_1)
query_2 <- sqldf("select class,odor from mushrooms where bruises =='t'")
table(query_2)
#Distribution of variables,barplot,capcolor
ggplot(data=mushrooms, aes(x = cap.color, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
#Distribution of variables,pieplot,capcolor
data1<-data.frame(table(mushrooms$cap.color))
data1 = data1[order(data1$Freq, decreasing = TRUE),] 
myLabel = as.vector(data1$Var1)   
myLabel = paste(myLabel, "(", round(data1$Freq / sum(data1$Freq) * 100, 2), "%)", sep = "")
ggplot(data1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = data1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank())+ ggtitle("capcolor") 
#Distribution of variables,barplot,capshape
ggplot(data=mushrooms, aes(x = cap.shape, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
#Distribution of variables,pieplot,capshape
data2<-data.frame(table(mushrooms$cap.shape))
myLabel2 = as.vector(data2$Var1)   
myLabel2 = paste(myLabel2, "(", round(data2$Freq / sum(data2$Freq) * 100, 2), "%)", sep = "")
ggplot(data2, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = data2$Var1, labels = myLabel2)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) + ggtitle("capshape")

train01 <- mushrooms
# Calculate number of class for each variable
z<-cbind.data.frame(Var=names(train01), Total_Class=sapply(train01,function(x){as.numeric(length(levels(x)))}))
print(z)

# create 70:30 stratified split using caret between Train and Test.
set.seed(101)
sample = sample.split(train01$class, SplitRatio = .7)
x_train = subset(train01, sample == TRUE)
x_test = subset(train01, sample == FALSE)

#__________________________

y_train<-x_train$class
y_test <- x_test$class

x_train$class<-NULL
x_test$class<-NULL


# Create a stratified sample for repeated cv
cv.10.folds<-createMultiFolds(y_train,k=10,times=2)

# create a control object for repeated cv in caret
ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=2,index=cv.10.folds)

# Model 1: random forest
rf.1.cv<-train(x_train,y_train,method="rf",trControl=ctrl.1,tuneLength=3)
plot(varImp(rf.1.cv),main="Random Forest - Variable Importance Plot")
# predict on test set and see the confusion matrix
y_predicted<-predict(rf.1.cv,x_test)
df1<-data.frame(Orig=y_test,Pred=y_predicted)
confusionMatrix(table(df1$Orig,df1$Pred))


# Model 2: RPART model
rpart <-train(x=x_train,y=y_train,method="rpart",trControl=ctrl.1,tuneLength=3)
plot(varImp(rpart),main="RPART - Variable Importance Plot")
rpart.plot(rpart$finalModel) #<- creates the decision tree with better formatting
y2_predicted<-predict(rpart,x_test)
df2<-data.frame(Orig=y_test,Pred=y2_predicted)
confusionMatrix(table(df2$Orig,df2$Pred))  #<-100% accuracy


# Model 3: bayesglm model
bayesglm <- train(x_train, y_train, method = "bayesglm", trControl = ctrl.1, tuneLength=3)
plot(varImp(bayesglm), main = "bayesglm - Variable Importance plot")
y3_predicted <- predict(bayesglm, x_test)
df3 <- data.frame (Original = y_test, Predicted = y3_predicted)
confusionMatrix(table(df3$Original, df3$Predicted))


# Model 4: C5.0
c50_fit <- train(x_train, y_train, method ="C5.0Rules", trControl = ctrl.1, tuneLength=3)
plot(varImp(c50_fit), main = "c50_fit - Variable Importance plot")
y4_predicted <- predict(c50_fit, x_test)
df4 <- data.frame (Original = y_test, Predicted = y4_predicted)
confusionMatrix(table(df4$Original, df4$Predicted))

# Model 5: Conditional Tree
ctree <- train(x_train, y_train, method ="ctree", trControl = ctrl.1, tuneLength=3)
plot(varImp(ctree), main = "Conditional Tree - Variable Importance plot")
y5_predicted <- predict(ctree, x_test)
df5 <- data.frame (Original = y_test, Predicted = y5_predicted)
confusionMatrix(table(df5$Original, df5$Predicted))

# Comparing Models
results <- resamples(list(RF=rf.1.cv, RPART=rpart, BAYESGLM=bayesglm, C5.0Rules=c50_fit, CTREE=ctree))
bwplot(results)

#save the random forest result because it takes long time.
save(rf.1.cv , file = 'RandomForest.rda')


 



