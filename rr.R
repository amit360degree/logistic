install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("sqldf")
install.packages("car")
install.packages("xlsx")



library(data.table)
library(dplyr)
library(stringr)
library(sqldf)
library(car)
library(xlsx)

##Importing the file 
f_daydata<-fread("F:\\fnp\\01-fnpindstg_2018-09-01.tsv\\01-fnpindstg_2018-09-01.tsv", header=FALSE, sep="\t")


View(f_daydata)

##Creating visitor id
f_daydata$visitor_id<-paste0(f_daydata$V1,f_daydata$V2,f_daydata$V3)


##Providing readable names
setnames(f_daydata,old=c("V4","V5","V6","V7","V8","V9","V10","V11","V12"), new=c("va_closer_id","va_closer_detail","visit_num","visit_page_num","post_event_list","hit_time_gmt","geo_region","mobile_id","post_pagename"
))


##aggregrating data
f_daydata_final<- f_daydata %>%
  group_by(visitor_id) %>%
  
  summarize(
    hit_count = n(),
    visits=(max(visit_num)-min(visit_num))+1,
    total_pages=last(visit_page_num),
    time_spend=max(hit_time_gmt)-min(hit_time_gmt),
    last_touch_point=last(va_closer_id),
    entry_page=first(post_pagename),
    second_entry_page=nth(post_pagename,2),
    exit_page=last(post_pagename),
    second_exit_page=nth(post_pagename,-2),
    geo_location=first(geo_region),
    mobile_id=first(mobile_id)
  )

##Second Day data Manipulation to find Revisit


s_daydata<-fread("F:\\fnp\\01-fnpindstg_2018-09-02.tsv\\01-fnpindstg_2018-09-02.tsv", header=FALSE, sep="\t")
View(s_daydata)

##Creating visitor Id
s_daydata$visitor_id<-paste0(s_daydata$V1,s_daydata$V2,s_daydata$V3)

##Setting friendly names
setnames(s_daydata,old=c("V4","V5","V6","V7","V8","V9","V10","V11","V12"), new=c("va_closer_id","va_closer_detail","visit_num","visit_page_num","post_event_list","hit_time_gmt","geo_region","mobile_id","post_pagename"
))

##Data Manipulation
s_daydata_final<- s_daydata %>%
  group_by(visitor_id) %>%
  
  summarize(
    hit_count = n(),
    visits=(max(visit_num)-min(visit_num))+1,
    total_pages=sum(visit_page_num),
    time_spend=max(hit_time_gmt)-min(hit_time_gmt),
    last_touch_point=last(va_closer_id),
    entry_page=first(post_pagename),
    second_entry_page=nth(post_pagename,2),
    exit_page=last(post_pagename),
    second_exit_page=nth(post_pagename,-2),
    geo_location=first(geo_region),
    mobile_id=first(mobile_id)
  )


##Providing Alias names to the dataframes befre joining the tables
s<-s_daydata_final
f<-f_daydata_final







##Finding revisit Id using left join

tem<-sqldf("Select f.visitor_id,
           case 
           when s.visitor_id is null then 0
           else 1
           end revisit
           
           From f LEFT JOIN s ON f.visitor_id = s.visitor_id")

##Joining both the tables
Firstday_data<-left_join(f_daydata_final,tem,by=c("visitor_id"))
View(Firstday_data)  

##Replacing entry page with second entry page where first entry page is empty
Firstday_data$actual_entrypage<-ifelse(Firstday_data$entry_page=="",Firstday_data$second_entry_page,
                                       Firstday_data$entry_page)



##Replacing exit page with second exit page where first exit page is empty


Firstday_data$actual_exitpage<-ifelse(Firstday_data$exit_page=="",Firstday_data$second_exit_page,
                                      Firstday_data$exit_page)

##Changing entry and exit pagenames

library(stringr)
Firstday_data$entry_pagename <- str_extract(Firstday_data$actual_entrypage, "plp|pdp|home|checkout|control|account")

Firstday_data$exit_pagename<- str_extract(Firstday_data$actual_exitpage, "plp|pdp|home|checkout|control|account")

View(Firstday_data)



##Removing missing values from entry and exit pagename
s4<-filter(Firstday_data, is.na(entry_pagename) | is.na(exit_pagename))
View(s4)
s5<-Firstday_data
finalData<-subset(s5,!(is.na(s5["entry_pagename"]) | is.na(s5["exit_pagename"])))
View(finalData)

##Changing geo location to region wise

s6<-filter(finalData,geo_location=="")

View(s6)

##Creating user defined function



var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}





#Vector of numerical variables

num_var= sapply(finalData,is.numeric)

Other_var= !sapply(finalData,is.numeric)

#Applying above defined function on numerical variables

my_num_data<-t(data.frame(apply(finalData[num_var], 2, var_Summ)))

my_cat_data<-data.frame(t(apply(finalData[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)

##Converting entry pagename and exit pagename to factor
finalData$entry_pagename<-factor(finalData$entry_pagename)

finalData$exit_pagename<-factor(finalData$exit_pagename)



##setting the model
set.seed(123)
#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(finalData), size = floor(0.70 * nrow(finalData)))

training<-finalData[train_ind,]
testing<-finalData[-train_ind,]
names(training)

View(testing)


#Building Models for training dataset

fit<-glm(revisit~hit_count+visits+total_pages+time_spend+last_touch_point+entry_pagename+exit_pagename,data = training,
         family = binomial(logit))



fit2<-glm(revisit~hit_count+visits+time_spend+entry_pagename+exit_pagename,data =training,
          family = binomial(logit))



fit3<-glm(revisit~hit_count+visits+time_spend+entry_pagename+exit_pagename,data =testing,
          family = binomial(logit))


##Output

summary(fit2)
coeff<-fit2$coef
write.csv(coeff, "coeff.csv")
fit2$fitted 
fit2$resid 
fit2$effects 
anova(fit2)
anova(fit2, test="Chisq")
Concordance(fit2)
source("F:\\Linear & Logistic In R\\Linear & Logistic In R\\Concordance.R")



################################VALIDATION ##############################
#Decile Scoring for 
##Training dataset
train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)



##Creatng Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)


###Decile Analysis Reports
fit_train_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                      , max(Prob) as max_prob 
                      , sum(revisit) as revisit_cnt
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)


summary(train1$decile)
xtabs(~decile,train1)




#Decile Scoring for 
##Training dataset
test1<- cbind(testing, Prob=predict(fit3, type="response")) 
View(test1)




##Creatng Deciles

decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))


fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob 
                     , sum(revisit) as revisit_cnt
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)



summary(test1$decile)
xtabs(~decile,test1)



with(fit2, null.deviance - deviance)



with(fit2, df.null - df.residual)




#Finally, the p-value can be obtained using:
with(fit2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



logLik(fit2)
