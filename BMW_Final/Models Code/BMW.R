#read data
library(data.table)
Sample09_15<-fread("C:\\Users\\kilo\\Desktop\\2017-09-15.csv")
sample09_15<-fread("C:\\Users\\kilo\\Desktop\\visitor_2017-09-15.csv")
#process data
q<-Sample09_15[,3:14]
w<-Sample09_15[,16:17]
Sample09_15<-cbind(q,w)
c=c('post_evar2','post_evar5','post_evar6','post_evar7','post_evar8','post_evar10','post_evar14','post_evar20','post_evar22','post_evar34','post_evar35','post_evar36','200','201')
names(Sample09_15)=c
Sample09_15<-Sample09_15[2:nrow(Sample09_15),]
Sample09_15$lead_type=0
#lead type
Sample09_16[which(Sample09_16[,'200']==0 & Sample09_16[,'201']==0),15]=1
Sample09_16[which(Sample09_16[,'200']==1 & Sample09_16[,'201']==0),15]=2
Sample09_16[which(Sample09_16[,'201']==1),15]=3
Sample09_16<-Sample09_16[,-(13:14)]


#combine two table
Sample09_16<-cbind(Sample09_16,sample09_16)
Sample09_16<-Sample09_16[,-14]
Sample09_16<-na.exclude(Sample09_16)
#Sampling
length(which(Sample09_16[,'lead_type']==1))
length(which(Sample09_16[,'lead_type']==2))
length(which(Sample09_16[,'lead_type']==3))
sample1<-Sample09_16[sample(which(Sample09_16$lead_type==1),size=length(which(Sample09_16[,'lead_type']==3))),]
sample2<-Sample09_16[sample(which(Sample09_16$lead_type==2),size=length(which(Sample09_16[,'lead_type']==3))),]
sample3<-Sample09_16[Sample09_16$lead_type==3,]
Sample09_16<-rbind(sample1,sample2,sample3)

#Create frequency

Sample09_16$vistor_fre=0
#if interger
for(i in 1:nrow(Sample09_16)){
  Sample09_16$vistor_fre[i]=sum(Sample09_16$hourly_visitor[i], Sample09_16$daily_visitor[i]
                                , Sample09_16$weekly_visitor[i],Sample09_16$monthly_visitor[i]
                                , Sample09_16$quarterly_visitor[i],Sample09_16$yearly_visitor[i])
}
#if character
for(i in 1:nrow(Sample10_22)){
  Sample10_22$vistor_fre[i]=sum(Sample10_22$hourly_visitor[i], Sample10_25$daily_visitor[i]
                                 , Sample10_25$monthly_visitor[i]
                                 , Sample10_25$quarterly_visitor[i],Sample10_25$yearly_visitor[i])
}

for(i in 1:nrow(Sample10_25)){
  if (Sample10_25$weekly_visitor[i]=="0"){
    Sample10_25$vistor_fre[i]=Sample10_25$vistor_fre[i]
  }else{
    Sample10_25$vistor_fre[i]=Sample10_25$vistor_fre[i]+1
  }
}
#Cut column
Sample09_16<-Sample09_16[,-(14:19)]
rm(sample09_16)
#DT
levels(Ori$post_evar2)
levels(Ori$post_evar5)
levels(Ori$post_evar6)
levels(Ori$post_evar7)
levels(Ori$post_evar10)
levels(Ori$post_evar14)
levels(Ori$post_evar22)
tree_1 = rpart(lead_type ~ Post_evar5+Post_evar6+Post_evar7+Post_evar22+Post_evar34+Post_evar35+Post_evar36+vistor_fre,data=train,method='class',minbucket=100)
rpart.plot(tree_1)
summary(tree_1)
#Logistic Model
model<-pred.GLM1=data.frame(lead_type=train$lead_type,Pred=predict(model,type="response"))
pred.GLM1$PredClass=ifelse(pred.GLM1$Pred > 0.5, "1","0")
pred.GLM1[1:10,]glm(lead_type~Post_evar2+Post_evar5+Post_evar6+Post_evar7+Post_evar10+Post_evar14+Post_evar22+vistor_fre,family=binomial(logit),data=train)

#_______________________________________________________________
#read data
library(data.table)
sample_1<-fread("C:\\Users\\kilo\\Desktop\\16sept.csv")
q<-sample_1[,3:14]
w<-sample_1[,16:17]
sample_1<-cbind(q,w)
#name data
c=c('post_evar2','post_evar5','post_evar6','post_evar7','post_evar8','post_evar10','post_evar14','post_evar20','post_evar22','post_evar34','post_evar35','post_evar36','200','201')
names(sample_1)=c
sample_1<-sample_1[2:nrow(sample_1),]
sample_1$lead_type=0

#lead type
sample_1[which(sample_1[,'200']==0 & sample_1[,'201']==0),15]=1
sample_1[which(sample_1[,'200']==1 & sample_1[,'201']==0),15]=2
sample_1[which(sample_1[,'201']==1),15]=3
sample_1<-sample_1[,-(13:14)]
#length of each type
length(which(sample_1[,'lead_type']==1))
length(which(sample_1[,'lead_type']==2))
length(which(sample_1[,'lead_type']==3))
#Sample
sample1<-sample_1[sample(which(sample_1$lead_type==1),size=length(which(sample_1[,'lead_type']==3))),]
sample2<-sample_1[sample(which(sample_1$lead_type==2),size=length(which(sample_1[,'lead_type']==3))),]
sample3<-sample_1[sample_1$lead_type==3,]
sample_1<-rbind(sample1,sample2,sample3)
sample<-rbind(sample,sample_1)
# out put csv
write.csv(sample,file="C:\\Users\\kilo\\Desktop\\sample.csv",quote=F,row.names = F)

#turn na to null
sample[is.na(sample)] <- " "
#change to factor
sample=data.frame(sample)
sample[sapply(sample,is.character)]<-lapply(sample[sapply(sample,is.character)],as.factor)
#transform factor to number
sample$Post_evar36=0
k<-as.factor(levels(sample$post_evar36))
for(i in 1:nrow(sample)){
  sample$Post_evar36[i]=which(k==sample$post_evar36[i])
}



