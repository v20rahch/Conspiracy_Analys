library(class)
library(MASS)
library(leaps)
library(caret)
library(caTools)
#import dataset
df <- read.delim('/Users/rahulc/Documents/Statistical Learning/HA1/Conspiracy.txt')
class(df)
str(df)
unique(df$white)
table(df$age) #
str(df)
summary(df)
nrow(df)
sum(complete.cases(df))
sum(!complete.cases(df))
#View(df)
#plot(df1) # Useless
# Handling missing data 
df1<-df
#View(df1)
summary(df1) 
df1$weight <- NULL # remove column as advised

sum(complete.cases(df1))
sum(!complete.cases(df1))
table(df1$hhi)
#Median values for 105 -idlg, pid3 - 50, 
#pid2-56,hhi-18,hispanic-19
df1$idlg<- ifelse(is.na(df1$idlg),
                  ave(df1$idlg,FUN=function(x) median(x,na.rm =TRUE)),
                  df1$idlg)
df1$pid3<- ifelse(is.na(df1$pid3),
                  ave(df1$pid3,FUN=function(x) median(x,na.rm =TRUE)),
                  df1$pid3)
df1$pid2<- ifelse(is.na(df1$pid2),
                  ave(df1$pid2,FUN=function(x) median(x,na.rm =TRUE)),
                  df1$pid2)
df1$white<- ifelse(is.na(df1$white),
                  ave(df1$white,FUN=function(x) median(x,na.rm =TRUE)),
                  df1$white)
df1$hispanic<- ifelse(is.na(df1$hispanic),
                   ave(df1$hispanic,FUN=function(x) median(x,na.rm =TRUE)),
                   df1$hispanic)
df1$hhi<- ifelse(is.na(df1$hhi),
                      ave(df1$hhi,FUN=function(x) median(x,na.rm =TRUE)),
                      df1$hhi)

df1 <-na.omit(df1) # remove pending NA Values

sum(complete.cases(df1))
sum(!complete.cases(df1))

# 1004 values remain in the dataset without any missing values, filled mostly 
#with median values considering many data as categorical

#STEP 2 - Convert Categorical data age, hispanic,white,pid3,pid2,
#and idlg

str(df1)

table(df1$md_localtv)

df1$gender<- as.factor(df1$gender)

df1$gender<- as.factor(df1$gender)
df1$cons_biowpn_dummy<- as.factor(df1$cons_biowpn_dummy) 
df1$cons_biowpn<- as.factor(df1$cons_biowpn) 
## Predictor for logistic regression
df1$white<- as.factor(df1$white)
df1$hispanic<- as.factor(df1$hispanic)
df1$pid3<- as.factor(df1$pid3)
df1$pid2<- as.factor(df1$pid2)
df1$idlg<- as.factor(df1$idlg)
# Inference
#Method - Q1

str(df1)
####

glm.fits=glm(cons_biowpn_dummy~trust_1+  
               populism_5+
               populism_4+
               populism_3+
               populism_2+
               populism_1+
               age+
               gender+
               hhi+
               hispanic+
               cov_beh_sum+
               cons_covax+
               white+
               highered+
               idlg+
               pid3+
               pid2+
               md_radio+
               md_national+
               md_broadcast+
               md_localpap+
               md_localtv+
               md_fox+
               md_agg+
               md_con+
               ms_news+
               rw_news,
  data=df1,
  family=binomial)


summary(glm.fits)
str(df1)
View(df1)
names(df1)
cor(df1$rw_news,df1$md_fox)
## Best subset Selection method for variable selection
regfit.full=regsubsets(cons_biowpn_dummy~trust_1+  
                        populism_5+
                        populism_4+
                        populism_3+
                        populism_2+
                        populism_1+
                        age+
                        gender+
                        hhi+
                        hispanic+
                        cov_beh_sum+
                        cons_covax+
                        white+
                        highered+
                        idlg+
                        pid3+
                        pid2+
                        md_radio+
                        md_national+
                        md_broadcast+
                        md_localpap+
                        md_localtv+
                        md_fox+
                        md_agg+
                        md_con+
                        ms_news,
                      data=df1 ,nvmax=26)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$bic

plot(reg.summary$bic ,xlab="Number of Variables ",
     ylab="BIC",type="l")

which.min(reg.summary$bic)
points(5,reg.summary$bic[5], col="red",cex=2,pch=20)
plot(regfit.full,scale="bic")
coef(regfit.full ,5)

## Variables Selection for model
glm.fits=glm(cons_biowpn_dummy~
               populism_5+
               populism_1+
               cons_covax+
               pid2+
               md_fox,
             data=df1,
             family=binomial)


summary(glm.fits)
coefficients(glm.fits)

vector <- c(-5.79306,
            0.35104,
            0.4639,
            1.05821,
            0.77425,
            0.36263)
exp(vector)

######## End of question 1
## Question 2 - QDA - 
## Best subset selection


set.seed(1)
split = sample.split(df1$cons_biowpn, SplitRatio = 0.8)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)
str(test_set)
## Best subset Selection method for variable selection
str(training_set)
regfit.full2=regsubsets(cons_biowpn~trust_1+  
                         populism_5+
                         populism_4+
                         populism_3+
                         populism_2+
                         populism_1+
                         age+
                         gender+
                         hhi+
                         hispanic+
                         cov_beh_sum+
                         cons_covax+
                         white+
                         highered+
                         idlg+
                         pid3+
                         pid2+
                         md_radio+
                         md_national+
                         md_broadcast+
                         md_localpap+
                         md_localtv+
                         md_fox+
                         md_agg+
                         md_con+
                         ms_news,
                       data=df1 ,nvmax=29)
reg.summary2=summary(regfit.full2)
names(reg.summary2)
reg.summary2$bic

plot(reg.summary2$bic ,xlab="Number of Variables ",
     ylab="BIC",type="l")

which.min(reg.summary2$bic)
points(6,reg.summary2$bic[6], col="red",cex=2,pch=20)
plot(regfit.full2,scale="bic")
coef(regfit.full2 ,6)
# qda.fit=qda(cons_biowpn~ 
#               populism_5+
#               populism_1+
#               cons_covax+
#               pid3+
#               md_localpap+
#               md_fox,
#             data=training_set)
# class(qda.fit)
# qda.pred=predict(qda.fit,test_set)
# qda.class=qda.pred$class
# qda.class
# table(qda.class,test_set[,12])
# 
# # mean(qda.class!=test_set[,12])
# mean(qda.class==test_set[,12])
# 
# cm = prop.table(table(qda.class,test_set[,12]),2)
# cm
########QDA with cross validation 

library(MASS)
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
qda.fit.cv = train(cons_biowpn ~ 
                     populism_5+
                     populism_1+
                     cons_covax+
                     pid3+
                     md_localpap+
                     md_fox, 
                   data=training_set,
                   method="qda",trControl=trctrl, tuneLength = 0)
qda.fit.cv

qda.pred=predict(qda.fit.cv,test_set)
table(qda.pred,test_set[,12])
#mean(qda.pred!=test_set[,12])
mean(qda.pred==test_set[,12])
cm = prop.table(table(qda.pred,test_set[,12]),2)
cm
### Fitting KNN to training set and predict test results
library(caTools)
library(caret)


set.seed(1)
split = sample.split(df1$cons_biowpn, SplitRatio = 0.8)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)

which(colnames(df1)=="cons_biowpn") # column no of dependent variable

# 10- Fold cross Validation for KNN 

ctrl <- trainControl(method="cv",number = 10,savePredictions=TRUE)  
knnFit <- train(cons_biowpn ~ populism_5+
                  populism_1+
                  cons_covax+
                  pid3+
                  md_localpap+
                  md_fox, data = training_set, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"))
knnFit
plot(knnFit)
knnPredict <- predict(knnFit,newdata = test_set )
confusionMatrix(knnPredict, test_set[,12] )
table(knnPredict, test_set[,12] )
prop.table(table(knnPredict,test_set[,12]),2)
mean(knnPredict==test_set[,12])




# KNN manually with some values of K
# 
# for (t in 1:10){
#   y_pred = knn(train = training_set[,-12],
#                test =test_set[,-12],
#                cl = training_set[,12],
#                k=t)
#   y_pred
#   table(y_pred,test_set[,12])
#   cm1 = table(y_pred,test_set[,12])
#   cm = prop.table(table(y_pred,test_set[,12]),2)
#   cm
#   
#   w=mean(y_pred==test_set[,12])
#   print(paste("The accuracy is ",w," for value of k ",t))
# }
# 

 

### LDA 

lda.fit = lda(cons_biowpn ~ 
                  populism_5+
                  populism_1+
                  cons_covax+
                  pid3+
                  md_fox, 
              data=training_set)
class(lda.fit)
lda.fit

lda.pred=predict(lda.fit,test_set)
lda.class=lda.pred$class
lda.class
table(lda.class,test_set[,12])

mean(lda.class!=test_set[,12])
mean(lda.class==test_set[,12])

cm = prop.table(table(lda.class,test_set[,12]),2)
cm
######### LDA With CV

training_set$pid3 <- as.numeric(training_set$pid3)
test_set$pid3 <- as.numeric(test_set$pid3)
library(MASS)

trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
lda.fit.cv = train(cons_biowpn ~ 
                  populism_5+
                  populism_1+
                  cons_covax+
                  pid3+
                  md_fox, 
                data=training_set,
                method="lda",trControl=trctrl, tuneLength = 0)
lda.fit.cv
lda.pred=predict(lda.fit.cv,test_set)

class(lda.pred)
class(test_set[,12])
table(lda.pred,test_set[,12])

#mean(lda.pred!=test_set[,12])
mean(lda.pred==test_set[,12])

cm = prop.table(table(lda.pred,test_set[,12]),2)
cm


