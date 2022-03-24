library(readxl)
library(dplyr)
library(naniar)
library(VIM)
library(caret)
library(class)
library(randomForest)
library(smotefamily)
library(rpart)
###import data
dat1<-read_excel("D:/2022_Study/ML/learning data/report 61-1.xlsx")
dat2<-read_excel("D:/2022_Study/ML/learning data/report 61-2.xlsx")
dat3<-read_excel("D:/2022_Study/ML/learning data/report 62-2.xlsx")
dat4<-read_excel("D:/2022_Study/ML/learning data/63-1.xlsx",sheet = "sec 1")
dat5<-read_excel("D:/2022_Study/ML/learning data/63-2.xlsx",sheet = "1")
dat1
glimpse(dat2)
summary(dat2)

####################
dat1<-dat1[,-c(1,3)]
names(dat1)[]<-c("midterm","att","prac","crit","proposal","project","grade")
dat2<-dat2[,c(5,10:14,16)]
names(dat2)[]<-c("midterm","att","prac","crit","proposal","project","grade")
dat3<-dat3[,c(2:7,12)]
names(dat3)[]<-c("midterm","att","prac","crit","proposal","project","grade")
dat4<-dat4[,c(5,10:14,17)]
names(dat4)[]<-c("midterm","att","prac","crit","proposal","project","grade")

dat<-rbind(dat1,dat2,dat3,dat4)
head(dat)
tail(dat)
glimpse(dat)
miss_var_summary(dat)
aggr(dat)
dat<-na.omit(dat)
table(is.na(dat))
dat$att<-round(dat$att/5*100)
dat$prac<-round(dat$prac/5*100)
mean_score<-(dat$crit+dat$project+dat$proposal)/40*20

dat<-cbind(dat[,1:3],mean_score,dat[,7])

for (i in 1:nrow(dat))
{
    if(dat$grade[i]=="D")
    {dat$group[i]<-1} else
        if(dat$grade[i]=="D+")
        {dat$group[i]<-1} else
        {dat$group[i]<-0}
}       
dat
glimpse(dat)
dat$grade<-as.factor(dat$grade)
dat$group<-as.factor(dat$group)
dat
class(dat)
###########################################################

set.seed(123)
train.id<-createDataPartition(dat$group,p=0.7,list = FALSE,times = 1)
train.dat<-dat[train.id,]
test.dat<-dat[-train.id,]
glimpse(train.dat)

###########################################
x.train<-train.dat[,c(-5,-6)]
y.train<-train.dat$group
glimpse(x.train)
x.train$midterm<-as.numeric(x.train$midterm)
x.train$att<-as.numeric(x.train$att)
x.train$prac<-as.numeric(x.train$prac)

smote<-SMOTE(x.train,y.train)
train.dat2<-smote$data
table(train.dat2$class)
train.dat2$class<-as.factor(train.dat2$class)

###########################################################
fit.rf<-randomForest(group~.,data = train.dat[,-5])
fit.rf
pred.rf<-predict(fit.rf,newdata= test.dat[,-5])
cm.rf<-confusionMatrix(pred.rf,as.factor(test.dat[,-5]$group))


###########################################################
fit.rf2<-randomForest(class~.,data = train.dat2)
fit.rf2
pred.rf2<-predict(fit.rf,newdata= test.dat[,-5])
cm2.rf2<-confusionMatrix(pred.rf,as.factor(test.dat[,-5]$group))

###########################################################
control<-trainControl(method="cv",number=10)
grid<-data.frame(k=1)
###########################################################
fit.knn2<-train(class~.,data = train.dat2,
               method = "knn",
               trControl=control,
               tuneGrid=grid)
pred.knn2<-knn(train.dat2,test.dat[,-5],train.dat2$class)
cm.knn2<-confusionMatrix(pred.knn2,as.factor(test.dat[,-5]$group))

save.image()

