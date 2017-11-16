#-------------------------------Q2--------------------------------------------------------
#LoanStats3c ->(loan.csv)
#LoanStats3d-> (2015.csv)
#Q2(a)
library(data.table)

data14<-fread("LoanStats3c.csv",skip=1,sep=",")
data15<-fread("LoanStats3d.csv",skip=1,sep=",")

data14<-head(data14,-4)

data15<-head(data15,-4)


k<-data.frame(data14$loan_amnt)  
names(k) <- c("loan_amnt")
p<-data.frame(data15$loan_amnt)
names(p)<-c("loan_amnt")

col_1415<-rbind(k,p)


h <- as.numeric(as.vector(col_1415$loan_amnt))
medianLoan<-median(h,na.rm=TRUE)
sprintf("median :%.10f",medianLoan)

-----------------------------------------------------------------------------------------
#Q2(b)

#data14<-fread("loan.csv",skip=1,sep=",")
data14<-head(data14,-4)
#data15<-fread("2015.csv",skip=1,sep=",")
data15<-head(data15,-4)

k<-data.frame(data14$purpose)
names(k) <- c("purpose")
p<-data.frame(data15$purpose)
names(p)<-c("purpose")

col_1415<-rbind(k,p)

count<-as.data.frame(table(col_1415$purpose))


summ<-sum(count$Freq)

common<-max(count$Freq)

#debt_consolidation is the most common purpose
ratio<-common/summ
ratio
library(MASS)
frac<-fractions(ratio)
frac
----------------------------------------------------------------------------------------------------------
#Q2(c)

#data14<-fread("loan.csv",skip=1,sep=",")
#data15<-fread("2015.csv",skip=1,sep=",")

data14$int_rate <- as.numeric(as.character(gsub("%", "", paste(data14$int_rate))))
data15$int_rate <- as.numeric(as.character(gsub("%", "", paste(data15$int_rate))))

int_avg14<- data.frame(data14$purpose, data14$int_rate)
names(int_avg14) <- c("Purpose", "int_rate")
int_avg15 <- data.frame(data15$purpose, data15$int_rate)
names(int_avg15) <- c("Purpose", "int_rate")
int_avg <- rbind(int_avg14, int_avg15)
int_avg$Avg_Rate <- as.numeric(as.character(int_avg$int_rate))

Rate <- aggregate(Avg_Rate ~ Purpose, int_avg, mean)
ratio <- min(Rate$Avg_Rate)/max(Rate$Avg_Rate)
sprintf("Ratio of minimum average rate to the maximum average rate: %.10f", ratio)

------------------------------------------------------------------------------------------
#Q2(d)

data14<-fread("LoanStats3c.csv",skip=1,sep=",")[,'term']

data14<-data14[!apply(data14 == "", 1, all),]
count1<-as.data.frame(table(data14$term))


summ<-sum(count1$Freq)

common<-max(count1$Freq)

ratio1<-common/summ

data15<-fread("LoanStats3d.csv",skip=1,sep=",")[,'term']

data15<-data15[!apply(data15 == "", 1, all),]
count2<-as.data.frame(table(data15$term))


summ<-sum(count2$Freq)

common<-max(count2$Freq)

ratio2<-common/summ



diff=ratio1-ratio2
diff
frac<-fractions(diff)
frac
----------------------------------------------------------------------------------------------------------
#Q2(e)

#data14<-fread("loan.csv",skip=1,sep=",")
#data15<-fread("2015.csv",skip=1,sep=",")

m <- data.frame(data14$loan_status, data14$issue_d,data14$last_pymnt_d,data14$term)
names(m) <- c("loan_status", "issue_d","last_pymnt_d","term")
n<- data.frame(data14$loan_status, data14$issue_d,data14$last_pymnt_d,data14$term)
names(n) <- c("loan_status", "issue_d","last_pymnt_d","term")
col_1415<- rbind(m,n)

r<-subset(col_1415,loan_status!="Fully Paid")

r1<-subset(r,loan_status!="Current")


r2<-subset(r1,loan_status!="In Grace Period")


library(zoo)
library(stringr)

r2$issue_d<-as.Date(r2$issue_d,"%b-%d")
r2$issue_d<-str_sub(r2$issue_d, start=6, end=11)
r2$last_pymnt_d<-as.Date(r2$last_pymnt_d,"%b-%d")
r2$last_pymnt_d<-str_sub(r2$last_pymnt_d, start=6, end=11)


r2$date_diff <- as.Date(as.character(r2$last_pymnt_d), format="%m-%d")-
                  as.Date(as.character(r2$issue_d), format="%m-%d")

r2$date_diff<-abs(r2$date_diff)

r2$date_diff<- as.numeric(as.character(gsub("days", "", paste(r2$date_diff))))
r2$term<- as.numeric(as.character(gsub("months", "", paste(r2$term))))
r2$date_diff<-r2$date_diff/12




Time_spent<-(r2%>%
 mutate(ratio=date_diff/term))

sdd<-sd(Time_spent$ratio,na.rm=TRUE)

sprintf("standard deviation : %.10f",sdd)

#--------------------------------------------------------------------------------------------------

#Q2(f)

#data14<-fread("loan.csv",skip=1,sep=",")
#data15<-fread("2015.csv",skip=1,sep=",")

data14$int_rate <- as.numeric(as.character(gsub("%", "", paste(data14$int_rate))))
data15$int_rate <- as.numeric(as.character(gsub("%", "", paste(data15$int_rate))))

h <- data.frame(data14$total_pymnt, data14$int_rate,data14$loan_amnt,data14$loan_status)
names(h) <- c("total_pymnt","int_rate","loan_amnt","loan_status")
k <- data.frame(data15$total_pymnt, data15$int_rate,data15$loan_amnt,data15$loan_status)
names(k) <- c("total_pymnt","int_rate","loan_amnt","loan_status")

col_1415<-rbind(h,k)

col_1415<-subset(col_1415,loan_status=="Fully Paid")

Final<-(col_1415%>%
 mutate(rate_of_return=(total_pymnt-loan_amnt)))

myFun <- function(x) {
  c(mean = mean(x,na.rm=TRUE), median = median(x,na.rm=TRUE),variance=var(x,na.rm=TRUE),
     iqr=IQR(x),skewness=skewness(x),kurtosis=kurtosis(x), 
    mean_abs_deviation=aad(x,na.rm=TRUE))
}
library(moments)
library(lsr)
myFun(Final$rate_of_return)

#------------------------------------------------------------------------------------------------------

#Q2(g)

#data14<-fread("loan.csv",skip=1,sep=",")
#data15<-fread("2015.csv",skip=1,sep=",")

col14<-data.frame(data14$purpose,data14$addr_state)
names(col14)<-c("purpose", "addr_state")
col15<-data.frame(data15$purpose,data15$addr_state)
names(col15) <- c("purpose", "addr_state")
col_1415<- rbind(col14,col15)
col_1415<-col_1415[!apply(col_1415 == "", 1, all),]



library(plyr)
counts <- ddply(col_1415, .(col_1415$purpose), nrow)
names(counts) <- c("purpose", "Freq")
counts


library(plyr)
co<- ddply(col_1415, .(col_1415$addr_state,col_1415$purpose), nrow)
names(co) <- c("addr_state","purpose", "Freq")
co



co<-subset(co,Freq>=10)


an<-mutate(co,ratio=co$Freq/sum(co$))

an
edit(an)

ans=max(an$ratio)

ans


#------------------------------------------------------------------------------------------------------------

Q2(h)

#Avg interest rate

#data14<-fread("loan.csv",skip=1,sep=",")
#data15<-fread("2015.csv",skip=1,sep=",")

data14$int_rate <- as.numeric(as.character(gsub("%", "", paste(data14$int_rate))))
data15$int_rate <- as.numeric(as.character(gsub("%", "", paste(data15$int_rate))))

int_avg14 <- data.frame(data14$sub_grade, data14$int_rate)
names(int_avg14) <- c("sub_grade", "int_rate")
int_avg15 <- data.frame(data15$sub_grade, data15$int_rate)
names(int_avg15) <- c("sub_grade", "int_rate")
int_avg <- rbind(int_avg14, int_avg15)
int_avg$Avg_Rate <- as.numeric(as.character(int_avg$int_rate))
Rate <- aggregate(Avg_Rate ~ sub_grade, int_avg, mean)
Rate


#percentage of loan status categories

col14<- data.frame(data14$sub_grade,data14$loan_status)
names(col14) <- c("sub_grade", "loan_status")
col15 <- data.frame(data15$sub_grade, data15$loan_status)
names(col15) <- c("sub_grade", "loan_status")

col_1415<- rbind(col14,col15)




col_1415<-col_1415[!apply(col_1415 == "", 1, all),]


library(plyr)
counts <- ddply(col_1415, .(col_1415$sub_grade,col_1415$loan_status), nrow)
names(counts) <- c("sub_grade", "loan_status", "Freq")
counts


n<-sum(counts$Freq)

Final<-(counts%>%
 mutate(per=Freq*100/n))

Final

------------------------------------------------------------------------------------------------------------





