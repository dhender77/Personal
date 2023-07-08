
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
library(car)
library(tidyverse)
rm(list=ls())

Hang = read.csv(choose.files(), header=T, stringsAsFactors = TRUE)
head(Hang)
summary(Hang$Nicotine)
summary(Hang$Lift)

colnames(Hang)[colnames(Hang) == 'Hang'] = 'Total'

colSums(is.na(Hang))


#Fill na's with mean value of column
Hang$Nicotine = as.numeric(ifelse(Hang$Nicotine == "N", 0, ifelse(Hang$Nicotine == "Y", 1,"")))
Hang$Lift = as.numeric(ifelse(Hang$Lift == "N", 0, ifelse(Hang$Lift == "Y", 1,"")))

Hang$Drink[is.na(Hang$Drink)] = mean(Hang$Drink, na.rm =  TRUE)
Hang$Smoke[is.na(Hang$Smoke)] = mean(Hang$Smoke, na.rm =  TRUE)
Hang$Wealth.Background[is.na(Hang$Wealth.Background)] = mean(Hang$Wealth.Background, na.rm =  TRUE)
Hang$Nicotine[is.na(Hang$Nicotine)] = mean(Hang$Nicotine, na.rm =  TRUE)
Hang$Lift[is.na(Hang$Lift)] = mean(Hang$Lift, na.rm =  TRUE)


colSums(is.na(Hang))

attach(Hang)

mean(Wealth.Background)
mean(Years)
round(mean(Age),digits = 2)
summary(Hometown)
mean(Hang[Hang$Hometown == "Texas", 'Age'])
mean(Hang[Hang$Hometown == "Minnesota", 'Score'])


Hometown_vector = unique(c(Hometown))
Hometown_vector

#summary statistics of mean age by Hometown

for (i in Hometown_vector){
  print(paste0(i,": ", round(mean(Hang[Hang$Hometown == i, 'Age']),digits = 2)))
}

#stepwise regression
m0 = lm(Total ~ 1)
head(Hang)
tail(Hang)

m1 = lm(Total ~ Years + Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports + Wealth.Background
        + Social.Confidence + Introvert.Extravert + Drink + Smoke + Nicotine + Lift
        )

step(m0, scope=list(lower=m0, upper=m1, direction ="both"))

m1step = lm(formula = Total ~ Religious + Years + Basketball)
summary(m1step)

#watching religious and basketball positive

#backwards regression
summary(m1)
m2 = lm(Score ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Drink + Smoke)
summary(m2)

m2a = lm(Score ~ Age + Political + Religious + Basketball + Baseball + Watch.Sports
         + Social.Confidence + Introvert.Extravert + Drink + Smoke)
anova(m2,m2a)

m3 = lm(Score ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Drink + Smoke)
summary(m3)

m4 = lm(Score ~ Age + Hometown + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Drink + Smoke)
summary(m4)

m5 = lm(Score ~ Age + Hometown + Religious + Basketball + Baseball + Watch.Sports
       + Drink + Smoke)
summary(m5)

m5 = lm(Score ~ Age + Hometown + Religious + Basketball + Watch.Sports
        + Drink + Smoke)
summary(m5)

m5 = lm(Score ~ Age + Hometown + Religious + Basketball + Baseball + Watch.Sports
        + Drink)
summary(m5)

m5 = lm(Score ~ Age + Hometown + Religious + Basketball + Watch.Sports
        )
summary(m5)

m5a = lm(Score ~ Age + Religious + Basketball + Watch.Sports
)
anova(m5,m5a)

m5 = lm(Score ~ Age + Hometown + Religious + Watch.Sports)
summary(m5)

m5 = lm(Score ~ Hometown + Religious + Watch.Sports)
summary(m5)

m5a = lm(Score ~  Religious + Watch.Sports)
anova(m5,m5a)

m5 = lm(Score ~ Religious + Watch.Sports)
summary(m5)

#Religion and Watch.Sports positive

#--------Drake----------

Drake = Hang[(Hang$Relation == "Drake"),]
summary(Drake)
Drake = Drake[,-which(names(Drake) %in% c("Relation"))] #Remove relation column
summary(Drake)
dim(Drake)

attach(Drake)

ggplot(data=Drake)+
  geom_histogram(aes(x= Hometown, fill = Friends), stat = "count", binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "State", y= "People")

ggplot(data=Drake)+
  geom_histogram(aes(x= Age), binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "Age", y= "People")

for (i in Hometown_vector){
  print(paste0(i,": ", round(mean(Drake[Drake$Hometown == i, 'Total']),digits = 2)))
}


m0 = lm(Total ~ 1)
m1 = lm(Total ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Drink + Smoke + Nicotine + Lift)
step(m0, scope=list(lower=m0, upper=m1, direction ="both"))

stepmodel = lm(formula = Total ~ Religious + Watch.Sports)
summary(stepmodel)

m2 = lm(Total ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Drink + Smoke)
summary(m2) #R2: .5165, AR2: .3305, RSE: 47.55

m2 = lm(Total ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Smoke)
summary(m2) #R2: .5164, AR2: .3431, RSE: 47.1

m2 = lm(Total ~ Age + Hometown + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Smoke)
summary(m2) #R2: .5159, AR2: .35451, RSE: 46.69

m2 = lm(Total ~ Age + Hometown + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert)
summary(m2) #R2: .5146, AR2: .3645, RSE: 46.33

m2 = lm(Total ~ Age + Hometown + Religious + Basketball + Watch.Sports
        + Social.Confidence + Introvert.Extravert)
summary(m2) #R2: .5087, AR2: .3683, RSE: 46.19

m2 = lm(Total ~ Age + Hometown + Religious + Basketball + Watch.Sports + Introvert.Extravert)
summary(m2) #R2: .502, AR2: .3709, RSE: 46.1

m2 = lm(Total ~ Age + Hometown + Religious + Watch.Sports + Introvert.Extravert)
summary(m2) #R2: .502, AR2: .3709, RSE: 46.1

m2a = lm(Total ~ Age + Religious + Watch.Sports + Introvert.Extravert)

anova(m2a,m2)

m2 = lm(Total ~ Age + Religious + Watch.Sports + Introvert.Extravert)
summary(m2) #R2: .4263, AR2: .3926, RSE: 45.29

m2 = lm(Total ~ Religious + Watch.Sports + Introvert.Extravert)
summary(m2) #R2: .4162, AR2: .3908, RSE: 45.36
#add back age

m2 = lm(Total ~ Age + Religious + Watch.Sports)
summary(m2) #R2: .4147, AR2: .3893, RSE: 45.42
#add back Introvert.Extravert

finalmodel = lm(Total ~ Age + Religious + Watch.Sports + Introvert.Extravert)
summary(finalmodel)


#---------Friends ---------

gm0 = glm(Friends ~ 1, family = binomial(link = "logit"))
gm1 = glm(Friends ~ Age + Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
        + Social.Confidence + Introvert.Extravert + Drink + Smoke + Nicotine + Lift
        ,family = binomial(link = "logit"))
summary(gm1)
step(gm0, scope=list(lower=gm0, upper=gm1, direction ="both"))

stepgm = glm(formula = Friends ~ Social.Confidence + Political + Watch.Sports
             , family = binomial(link = "logit"))
summary(stepgm)

exp(-6.308)/(exp(-6.308)+1)

RNGkind(sample.kind = "default")
set.seed(runif(n=1, min=1, max=10000))
train.idx <- sample(x=1:nrow(Drake), size=floor(.85*nrow(Drake)))
train.df<-Drake[train.idx,]
test.df<-Drake[-train.idx,]


mtry<-c(1:12)


keeps<-data.frame(m=rep(NA, length(mtry)), OOB_error_rate=rep(NA,length(mtry)))

for(ii in 1:length(mtry)){
  print(paste0("This is tree: ", mtry[ii]))
  tempforest<-randomForest(Friends~ Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
                           + Social.Confidence + Introvert.Extravert + Drink + Smoke + Nicotine + Lift
                           , data=train.df, ntree=1000, mtry=mtry[ii])
  
  keeps[ii, "m"]<- mtry[ii]
  keeps[ii, "OOB_error_rate"] <- mean(predict(tempforest)!=train.df$Friends)
  
}

rocCurve<-ggplot(keeps)+ geom_line(aes(x=m, y=OOB_error_rate))+ scale_x_continuous(breaks=mtry)+labs(x="m(mtry) value", y="OOB Error rate")
print(rocCurve)
#Use n=2 for final forest

finalforest<- randomForest(Friends ~ Hometown + Political + Religious + Basketball + Baseball + Watch.Sports
                           + Social.Confidence + Introvert.Extravert + Drink + Smoke + Nicotine + Lift
                           , data=train.df, ntree= 1000, mtry=1, importance=TRUE)
print(finalforest)

#OOB accuracy
(40+6)/(40+6+12+4)
#0.7419355


#OOB error
#25.81%

pi_hat<- predict(finalforest, test.df, type="prob")[,"Y"]

rocCurve<- roc(response=test.df$Friends, predictor= pi_hat, levels=c("N", "Y"))
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)

#Specificity: .857 (True Positive)
#Sensitivity: .5 (True Negative)
#AUC: .607
#the second goal is interpretation: understanding how the x variable impact the y variable
# A random forest can help us understand which x variables are the most important. IT does this by giving a 
# variable importance plot. 
#NOTE: you must have importance = TRUE
# in the forest code so that R collects the necessary data

varImpPlot(finalforest, type=1)
#Social Confidence, Religious, Political


summary(stepgm)
gm2 = glm(Friends ~ Political + Religious + Watch.Sports+ Drink + Nicotine + Baseball + Basketball + Smoke + Lift
          ,family = binomial(link = "logit"))
summary(gm2)

#run your own to test to find probability of being friends
new = data.frame(Religious=1.5, Watch.Sports= 3,  Drink = 1, Political = 4)
new
exp(predict(stepgm,new))/(exp(predict(stepgm,new))+1)
Hang[Hang$Name == "Jake Haley",]

name = Drake[Name == "Charles DeMerit",]
print(name)
exp(predict(stepgm,name))/(exp(predict(stepgm,name))+1)


#% chance of being friends based on glm
namevector = Drake[,1]
model = stepgm
for (i in namevector){
  name1 = Drake[Name == i,]
  print(paste0(i,": ",round((exp(predict(model,name1))/(exp(predict(model,name1))+1))*100,digits = 2),"%"))
}


#predict friend or not using random forest model
inputdata = data.frame(Drake)
inputdata
inputdata$predictedlabel <- predict(finalforest, newdata=inputdata)
print(paste0(inputdata$Name, ": Yes or No? ", inputdata$predictedlabel))

#predict on Eagan friends
print(paste0(Hang[Hang$Relation == "Eagan", 1], ": ",predict(finalforest, Hang[Hang$Relation == "Eagan",])))
