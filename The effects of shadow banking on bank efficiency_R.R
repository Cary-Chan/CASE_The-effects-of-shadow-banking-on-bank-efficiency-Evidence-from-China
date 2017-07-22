install.packages("frontier")
install.packages("lmtset") 
install.packages("micEcon") 
install.packages("zoo") 
install.packages("plm") 

library(frontier) 
library(lmtest)
library(micEcon) 
library(zoo) 
library(Formula) 
library(plm) 

#Input data. 
data1<-read.table(file="/Users/chenchuandong/Desktop/thesis/input-output参考文献/data-MasterThesis.csv", header=TRUE, sep= ";", dec=".", na.string="NA") summary(data1) 
data2<-plm.data(data1,c("BankNum","YEAR")) 
summary(data2) 

tc<-data2$TC 
loans<-data2$LOAN 
la<-data2$LA
oea<-data2$OEA 
dstf<-data2$DSTF
pf1<-data2$PF1
pc<-data2$PC
z1<-data2$Z1
z2<-data2$Z2
tea<-data2$TEA
ta<-data2$TA
wto<-data2$WTO 
cbrc<-data2$CBRC 
fc<-data2$FC 
year<-as.numeric(data2$YEAR) 

#Descriptive statistics of the outputs and inputs variables. 
mean(tc); max(tc); min(tc); sd(tc)
mean(loans); max(loans); min(loans); sd(loans) 
mean(dstf); max(dstf); min(dstf); sd(dstf) 
mean(pf1); max(pf1); min(pf1); sd(pf1) 
mean(pc); max(pc); min(pc); sd(pc) 

#Build model_1 (the original model). 
model_1<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) 
+I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/ta)*year)+I(log(dstf/ta)*year)+I(log(pf1/pc)*year) 
|z1+z2+wto+fc+cbrc+log(ta)+log(la/ta)+I(loans/dstf),data=data2,ineffDecrease=FALSE) 
summary(model_1) 
lrtest(model_1) 

model_1_0<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) +I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/ta)*year)+I(log(dstf/ta)*year)+I(log(pf1/pc)*year) 
|~1, data=data2,ineffDecrease=FALSE) 
#LR test between model_1_0 and model_1. 
lrtest(model_1_0,model_1) 

#Monotonicity calculation. 
h11<- (-0.37112606 -0.42413170*log(loans/ta)+0.73614386*log(dstf/ta)-0.00705085*log(pf1/pc)+0.01117596*year+ 0.47179289*(loans/dstf))*(tc/(pc*ta))/(loans/ta) 

h22<-(1.71972508 + 0.42922377*log(dstf/ta)+ 0.73614386*log(loans/ta)-0.00705780*log(pf1/ pc)-0.02030937*year-0.47179289*loans/dstf)*(tc/(pc*ta))/(dstf/ta) 

h33<-( 0.78636596-0.00639911*log(pf1/pc)-0.00705085*log(loans/ta)-0.00705780*log(dstf/ta) +0.00579885*year)*(tc/(pc*ta))/(pf1/pc) 

prec11<-(sum(h11<0)+sum(h11=0))/(sum(h11<0)+sum(h11=0)+sum(h11>0)) 
prec22<-(sum(h22<0)+sum(h22=0))/(sum(h22<0)+sum(h22=0)+sum(h22>0)) 
prec33<-(sum(h33<0)+sum(h33=0))/(sum(h33<0)+sum(h33=0)+sum(h33>0)) 

#Cost efficiency score calculation. 
eff_1<-efficiencies(model_1) 
RobustnessTestModel_1<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) +I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2) 
+I(log(loans/ta)*year)+I(log(dstf/ta)*year) +I(log(pf1/pc)*year)|z1+z2, 
data=data2,ineffDecrease=FALSE) 
summary(RobustnessTestModel_1) 

#LR test between model_1_0 and RobustnessTestModel_1. 
lrtest(model_1_0,RobustnessTestModel_1) 

RobustnessTestModel_2<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) +I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2) +I(log(loans/ta)*year)+I(log(dstf/ta)*year) +I(log(pf1/pc)*year) 
|z1+z2+wto+fc+cbrc, data=data2,ineffDecrease=FALSE) 
summary(RobustnessTestModel_2) 

RobustnessTestModel_2_0<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) +I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/ta)*year)+I(log(dstf/ta)*year) +I(log(pf1/pc)*year) 
|wto+fc+cbrc, data=data2,ineffDecrease=FALSE) 

#LR test between RobustnessTestModel_2_0 and RobustnessTestModel_2. 
lrtest(RobustnessTestModel_2_0,RobustnessTestModel_2) 

RobustnessTestModel_3<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) +I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/ta)*year)+I(log(dstf/ta)*year) +I(log(pf1/pc)*year) 
|z1+z2+log(ta)+log(la/ta)+I(loans/dstf), data=data2,ineffDecrease=FALSE) 
summary(RobustnessTestModel_3) 

RobustnessTestModel_3_0<-sfa(log(tc/(pc*ta))~log(loans/ta)+log(dstf/ta)+log(pf1/pc) +I(0.5*log(loans/ta)^2) 
+I(0.5*log(dstf/ta)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/ta)*log(dstf/ta))+I(log(loans/ta)*log(pf1/pc)) 
+I(log(dstf/ta)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/ta)*year)+I(log(dstf/ta)*year) +I(log(pf1/pc)*year) 
|log(ta)+log(la/ta)+I(loans/dstf), data=data2,ineffDecrease=FALSE) 

#LR test between RobustnessTestModel_3_0 and RobustnessTestModel_3. 
lrtest(RobustnessTestModel_3_0,RobustnessTestModel_3) 

#Robustness test on normalization. 
RobustnessTestModel_4<-sfa(log(tc/(pc*tea))~log(loans/tea)+log(dstf/tea)+log(pf1/pc) +I(0.5*log(loans/tea)^2) 
+I(0.5*log(dstf/tea)^2)
+I(0.5*log(pf1/pc)^2) +I(log(loans/tea)*log(dstf/tea))+I(log(loans/tea)*log(pf1/pc)) +I(log(dstf/tea)*log(pf1/pc))
+year
+I(0.5*year^2)
+I(log(loans/tea)*year)+I(log(dstf/tea)*year) +I(log(pf1/pc)*year) 
|z1+z2+wto+fc+cbrc+log(ta)+log(la/ta)+I(loans/dstf), data=data2,ineffDecrease=FALSE) 
summary(RobustnessTestModel_4) 


