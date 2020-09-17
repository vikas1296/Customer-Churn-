#******************************************Logistic Regression Case Study************************************************

setwd("E:/BA360/R/Proactive Attrition Management-Logistic Regression Case Study")


# Importing the data

mydata1<-read.csv("logistic.csv")
#****************************************Data Analysis***************************************
str(mydata1)
View(mydata1)

# excluding variables `CUSTOMER` & `CSA`

mydata= subset(mydata1,select = -c(CUSTOMER,CSA))


## Create user defined function for descriptive analysis

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

# Vector of numerical variables

num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)
View(Other_var)
# Applying above defined function on numerical variables

my_num_data<-t(data.frame(apply(mydata[num_var], 2, var_Summ)))
my_cat_data<-t(data.frame(apply(mydata[Other_var], 2, var_Summ)))

View(my_num_data)
View(my_cat_data)

write.csv(my_num_data, file = "num_data_summary.csv")


# Missing values

apply(is.na(mydata[,]),2,sum)

mydata <- mydata[!is.na(mydata$CHURN),]


# Missing Value Treatment

mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
mydata[,Other_var] <- apply(data.frame(mydata[,Other_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})

# Outlier Treatment

M1_fun <- function(x){
  quantiles <- quantile(x, c(.01, .99 ),na.rm=TRUE )
  # Above line will calc the P1 and P99
  
  x[x < quantiles[1] ] <- quantiles[1]  # if value < P1, then P1
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P99, then P99
  x
}

mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 2, M1_fun) 


TESTDATA <- t(data.frame(apply(mydata[num_var], 2, var_Summ)))

write.csv(TESTDATA, file = "TESTDATA.csv")

# Correlation matrix

corrm<- cor(mydata[,num_var]) ### CORRELATION MATRIX
View(corrm)

write.csv(corrm, file = "corrm1.csv") 

#****************************************Feature Engineering **************************************************
# Selecting important categorical varibales using 'chisquare test'

freq_table <- table(mydata$CHURN, mydata$CHILDREN) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITA) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITAA) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITB) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITC) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITDE) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITGY) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITZ) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$PRIZMRUR) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$PRIZMUB) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$PRIZMTWN) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$REFURB) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$WEBCAP) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCPROF) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCCLER) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCCRFT) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCSTUD) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCHMKR) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCRET) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$OCCSELF) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MARRYYES) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MARRYNO) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MAILORD) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MAILRES) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MAILFLAG) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$TRAVEL) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$PCOWN) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$NEWCELLY) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$NEWCELLN) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$TRUCK) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$RV) 
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$CREDITCD) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$INCOME) #significant
chisq.test(freq_table)
freq_table <- table(mydata$CHURN, mydata$MCYCLE) 
chisq.test(freq_table)


# Variable reduction using step wise regression

fitt <- step(lm(CHURN ~ REVENUE
                +MOU
                +RECCHRGE
                +DIRECTAS
                +OVERAGE
                +ROAM
                +CHANGEM
                +CHANGER
                +DROPVCE
                +BLCKVCE
                +UNANSVCE
                +CUSTCARE
                +THREEWAY
                +MOUREC
                +OUTCALLS
                +INCALLS
                +PEAKVCE
                +OPEAKVCE
                +DROPBLK
                +CALLFWDV
                +CALLWAIT
                +MONTHS
                +UNIQSUBS
                +ACTVSUBS
                +PHONES
                +MODELS
                +EQPDAYS
                +AGE1
                +AGE2
                +CREDITA
                +CREDITAA
                +CREDITB
                +CREDITC
                +CREDITDE
                +PRIZMRUR
                +PRIZMUB
                +PRIZMTWN
                +REFURB
                +WEBCAP
                +OCCRET
                +MARRYNO
                +MAILORD
                +MAILRES
                +NEWCELLY
                +CREDITCD
                +INCOME, data = mydata), direction = "both")

summary(fitt)

# Transformed Variables

mydata$root_MOU <- sqrt(mydata$MOU)
mydata$root_EQPDAYS<- round(sqrt(mydata$EQPDAYS))
mydata$root_OVERAGE <- sqrt(mydata$OVERAGE)

# Dividing dataset into "training" and "testing"

testing<- mydata[(mydata$CHURNDEP==0.5),]
training <- mydata[(mydata$CHURNDEP!=0.5),]
testing$CHURNDEP <- NULL
training$CHURNDEP<- NULL
nrow(training)
nrow(testing)
#********************************************** Model Building *********************************************************
# Building Models for "training" dataset

fit<-glm(CHURN ~ REVENUE + root_MOU + RECCHRGE + root_OVERAGE + ROAM + 
           CHANGEM + CHANGER + DROPVCE + CUSTCARE + THREEWAY + 
           INCALLS + PEAKVCE + OPEAKVCE + DROPBLK + CALLWAIT + MONTHS + UNIQSUBS + 
           ACTVSUBS + PHONES + root_EQPDAYS + AGE1 + CREDITAA + 
           CREDITB + CREDITC + CREDITDE + PRIZMRUR + PRIZMUB + 
           REFURB + WEBCAP + MARRYNO + MAILRES + NEWCELLY ,data = training,
           family = binomial(logit))


# Output of Logistic Regression

summary(fit)
ls(fit)
fit$model

coeff<-fit$coef #Coefficients of model

write.csv(coeff, "coeff.csv")

# Multicollinierity Checking using VIF

library(car)
asd <- as.matrix(vif(fit))

write.csv(asd, "vif1.csv")


# Concordance checking

source("Concordance.R")

Concordance(fit)  ## concordance- 0.6221

# Running Stepwise regression

step1=step(fit, direction = "both")

summary(step1)

# Final Model

fit2<-glm(CHURN ~ REVENUE + root_MOU + RECCHRGE + root_OVERAGE + ROAM + 
            CHANGEM + CHANGER + DROPVCE + THREEWAY + 
            INCALLS + PEAKVCE + OPEAKVCE + DROPBLK + MONTHS + UNIQSUBS + 
            ACTVSUBS + PHONES + root_EQPDAYS + AGE1 +  
            CREDITB + CREDITC + CREDITDE + PRIZMUB + 
            REFURB + WEBCAP + MARRYNO + MAILRES + NEWCELLY,data = training,
          family = binomial(logit))

summary(fit2)

source("Concordance.R")

Concordance(fit2) ## concordance- 0.62175


# Multicollinierity Checking using VIF

coeff<-fit2$coef #Coefficients of model

write.csv(coeff, "coeff2.csv")

library(car)
asd2 <- as.matrix(vif(fit2))

write.csv(asd2, "vif2.csv")

# Running anova

anova(fit2,fit, test = 'Chisq')

# Writing model coefficients

write.csv(fit2$coefficients,"Final_model_coeff.csv")

# Getting the standardized beta coefficients

install.packages("QuantPsyc")
library(QuantPsyc)

stb= data.frame(lm.beta(fit2))

View(stb)

#*************************************VALIDATION ******************************************
#Decile Scoring 

## Training dataset

train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)


##Creating Deciles

decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), CHURN_cnt=sum(CHURN), 
                             non_CHURN_cnt=total_cnt -CHURN_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)


write.csv(decile_summ_train,"fit_train_DA1.csv",row.names = F)


##Testing dataset

test1<- cbind(testing, Prob=predict(fit2,testing, type="response"))
View(test1)


##Creating Deciles

decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)

test1$decile<-factor(test1$decile)
decile_grp<-group_by(test1,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), CHURN_cnt=sum(CHURN), 
                            non_CHURN_cnt=total_cnt -CHURN_cnt )
decile_summ_test<-arrange(decile_summ_test, desc(decile))
View(decile_summ_test)

write.csv(decile_summ_test,"fit_test_DA1.csv",row.names = F)


#**************************************************************************************************************************