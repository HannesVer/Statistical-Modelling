studentnumber = 712177
fulldata = read.csv("HousePrices.txt", sep = " ", header = TRUE)
digitsum = function(x) sum(floor(x/10^(0:(nchar(x)-1)))%%10)
set.seed(studentnumber)
mysum = digitsum(studentnumber)
if((mysum %% 2) == 0) {rownumbers = sample(1:327,150,replace=F)
} else {rownumbers = sample(309:585,150,replace=F)}
mydata = fulldata[rownumbers,]

attach(mydata)
detach(mydata)
head(mydata)
install.packages("SemiPar")
install.packages("nlme")
install.packages("methods")
install.packages("statmod")
install.packages("stats")
install.packages("graphics")
install.packages("RCurl")
install.packages("jsonlite")
install.packages("tools")
install.packages("utils")
install.packages('data.table')
install.packages("h2o")
install.packages("hglm")
install.packages("RLRsim")


library(mgcv)
library(SemiPar)
library(MASS)
library(ggplot2)
library("car")
library(epicalc)

y <- mydata$PriceHouse
x1 <- mydata$Region
x2 <- mydata$Province
x3 <- mydata$Shops
x4 <- mydata$Bankruptcies
x5 <- mydata$MeanIncome
x6 <- mydata$TaxForms
x7 <- mydata$HotelRestaurant
x8 <- mydata$Industries
x9 <- mydata$HealthSocial

names(mydata)

#PREPROCESSING

#removing outliers

#mod <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data = mydata)
#cooksd <- cooks.distance(mod)

#sample_size <- nrow(mydata)
#plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
#abline(h = 4/sample_size, col="red")  # add cutoff line
#text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")

#influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
#mydata <- mydata[-influential, ]

#correlation investigation

cor(mydata[,-(1:3)])

#distribution plots

qqnorm(y); qqline(y)

qqPlot(mydata$y)
qqPlot(mydata$PriceHouse,distribution='gamma',shape=2,scale=2)

qqPlot(y,distribution='gamma',shape=2,scale=2)
qqPlot(y,distribution='gamma',shape=1,scale=2)
qqPlot(y,distribution='gamma',shape=2,scale=1)
qqPlot(y,distribution='gamma',shape=3,scale=2)
qqPlot(y,distribution='gamma',shape=3,scale=3)
qqPlot(y,distribution='gamma',shape=7,scale=2)
qqPlot(y,distribution='gamma',shape=2,scale=5)
qqPlot(y,distribution='gamma',shape=8,scale=0.5)
qqPlot(y,distribution='gamma',shape=0.2,scale=10)

qqPlot(y,distribution='gamma',shape=8,scale=10)
qqPlot(y,distribution='gamma',shape=20,scale=15)

qqPlot(mydata$PriceHouse,distribution='lnorm') #Nfits quite well actually, so Gaussian family with logarithmic link
qqPlot(y,distribution='lnorm') #Nfits quite well actually, so Gaussian family with logarithmic link

qqPlot(y,distribution='logis') 

#setting categories

x1W= (x1=="Waals_Gewest")
x1B= (x1=="Brussels_Hoofdstedelijk_Gewest")
x2B= (x2=="Brussels_area")
x2N= (x2=="Namur")
x2L= (x2=="Liege")
x2BW= (x2=="Brabant_Wallon")
x2H= (x2=="Hainaut")
x2Lu= (x2=="Luxembourg")

#specifying factors

mydata$Region <- as.factor(mydata$Region)
mydata$Province <- as.factor(mydata$Province)
#mydata$x1 <- as.factor(mydata$x1)
#mydata$x2 <- as.factor(mydata$x2)

#identifying which variables will need smoothing

plot(x3,log(y)) #negative effect & defs not linear, could use a smoothing function here
plot(x4,y) #hard to tell effect
plot(x5,log(y)) # a clear positive effect, could use smoothing
plot(x6,y) #smooth
plot(x7,y) #don't smooth
plot(x8,y) #smooth
plot(x9,y) #smooth

#QUESTION 1

#1.A

fit0 <- spm(y~x1W+x1B+x2B+x2N+x2L+x2BW+x2H+x2Lu+x3+x4+x5+x6+x7+x8+x9,spar.method="ML")
summary(fit0)

  
#Error in chol.default(Ridge) : the leading minor of order 9 is not positive definite
#according to StackOverflow: The error you are seeing occurs when some of the eigenvectors of the matrix you are trying to operate on are not positive 
#(typically they'll be zero, or below some very small threshold); 
#this means, essentially, that your data are too noisy/small to estimate a full covariance matrix.

#indeed, when we count the non-nulvalues of these factors, these are much smaller than their length 150
sum(x2B != 0)  
sum(x2N != 0)  
sum(x2L != 0)  
sum(x2Lu != 0)  
sum(x2BW != 0)  
sum(x2H != 0)

#Therefore I try the GAM package, because the x2 variable is probably too important to leave out

?gam

fit1 <- gam(y~x2+x3+x4+x5+x6+x7+x8+x9,spar.method="ML",data=mydata)
summary(fit1)


fit2 = gam(y~1,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit2)

fit2bis = gam(y~x2,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit2bis)
fit2bisbis = gam(y~x1+x2,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit2bisbis)

fit3 = gam(y~x2+s(x3),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit3)  

fit4 = gam(y~x2+s(x3)+x4,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit4) 

fit4bis = gam(y~x2+s(x3)+s(x4),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit4bis)

fit5 = gam(y~x2+s(x3) + s(x5),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit5) 
  
fit5bis = gam(y~x2+s(x3) + x5,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit5bis) 

fit6 = gam(y~x2+s(x3) + s(x5) + s(x6),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit6) 

fit7 = gam(y~x2+s(x3) + s(x5) + s(x6) + x7,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit7) 

fit8 = gam(y~x2+s(x3) + s(x5) + s(x6) + x7 + x8,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit8)

#x8 needed a spline
fit8bis = gam(y~x2+s(x3) + s(x5) + s(x6) + x7 + s(x8),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit8bis)


#fit9 lowest AIC#BASE 
#fit9 acts as a base
fit9 = gam(y~x2+s(x3) + s(x5) + s(x6) + x7 + s(x8) + s(x9),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit9)

fit10 = gam(y~x2+s(x3) + s(x5) + s(x6) + x7 + s(x8) + x9,method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit10)
#so from fit9 to fit10, clear that x9 needs a spline (as I expected)

fit11 = gam(y~x2+x3+ s(x5) + s(x6) + x7 + s(x8) + s(x9),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit11)
#so x3 really needs a spline


fit12 = gam(y~x2+s(x3) + x5 + s(x6) + x7 + s(x8) + s(x9),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit12)

fit13 = gam(y~x2+s(x3) + x5 + x6 + x7 + s(x8) + s(x9),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
AIC(fit13)

AICvalues =  c(AIC(fit9),AIC(fit10),AIC(fit11),AIC(fit12),AIC(fit13))
AICvalues

#PART B
fitx3 = gam(y~s(x3),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
plot(fitx3)

fitx5 = gam(y~s(x5),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
plot(fitx5)

fitx6 = gam(y~s(x6),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
plot(fitx6)

fitx8 = gam(y~s(x8),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
plot(fitx8)

fitx9 = gam(y~s(x9),method='GCV.Cp',data=mydata,family=gaussian(link="log"))
plot(fitx9)



#PART C

#see final doc latex



#QUESTION 2
  


#2.A

#nulhyp: that the simpler model alone is enough
#see minute 12:00 lesson 11 part 3
x <- cbind(x6,x9)
R <- poly(x,4)
R


#series expansion
#model0 <- lm(y~1+x6+x9+(x6^2) + (x9^2),data=mydata)
#model1 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3],data=mydata)
#model2 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4],data=mydata)
#model3 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6],data=mydata)
#model4 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7],data=mydata)
#model5 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8],data=mydata)
#model6 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10],data=mydata)
#model7 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11],data=mydata)
#model8 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12],data=mydata)
#model9 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12]+ R[,13],data=mydata)
#model10 <- lm(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12]+ R[,13] + R[,14],data=mydata)


model0 <- gam(y~1+x6+x9+(x6^2) + (x9^2),data=mydata,family=gaussian(link="log"))
model1 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3],data=mydata,family=gaussian(link="log"))
model2 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4],data=mydata,family=gaussian(link="log"))
model3 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6],data=mydata,family=gaussian(link="log"))
model4 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7],data=mydata,family=gaussian(link="log"))
model5 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8],data=mydata,family=gaussian(link="log"))
model6 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10],data=mydata,family=gaussian(link="log"))
model7 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11],data=mydata,family=gaussian(link="log"))
model8 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12],data=mydata,family=gaussian(link="log"))
model9 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12]+ R[,13],data=mydata,family=gaussian(link="log"))
model10 <- gam(y~1+x6+x9+(x6^2) + (x9^2) + R[,3] + R[,4] + R[,6] + R[,7] + R[,8] + R[,10] + R[,11] + R[,12]+ R[,13] + R[,14],data=mydata,family=gaussian(link="log"))



T1 <- ((2/1)*(logLik(model1) - logLik(model0)))
T2 <- ((2/2)*(logLik(model2) - logLik(model0)))
T3 <- ((2/3)*(logLik(model3) - logLik(model0)))
T4 <- ((2/4)*(logLik(model4) - logLik(model0)))
T5 <- ((2/5)*(logLik(model5) - logLik(model0)))
T6 <- ((2/6)*(logLik(model6) - logLik(model0)))
T7 <- ((2/7)*(logLik(model7) - logLik(model0)))
T8 <- ((2/8)*(logLik(model8) - logLik(model0)))
T9 <- ((2/9)*(logLik(model9) - logLik(model0)))
T10 <- ((2/10)*(logLik(model10) - logLik(model0)))
T <- c(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)
max(T)
T

#pvalue:
cn = 4.18
m = 10
pvalue = 1-exp(-sum((1-pchisq((1:m)*cn,1:m))/(1:m)))
pvalue

#QUESTION 3

#Part A

library(lme4)
library(lattice)

#index=function(y,x6) coef(lm(y~x6))[1]
#type=c("g","p","r")
xyplot(y ~ x6|x2,data=mydata,type=c("g","p","r"),xlab ="number of tax forms",ylab = "Median house price")
levels(x2)
?xyplot

#Part B
library(RLRsim)

fit14 <- glmmPQL(y~x6+x7,random=list(x2=~1+x6),family=gaussian(link=log))
summary(fit14)
summary(fit14)$coefficients

#fit14bis because "var-cov" doesn't work when Province-specific slopes included
fit14bis <- glmmPQL(y~x6+x7,random=list(x2=~1),family=gaussian(link=log))
summary(fit14bis)
intervals(fit14bis,which="var-cov") #11:28 lesson 9 Part 2



#fit14lme <- lme(y~x6+x7,random=~1|x2,data = mydata)
#summary(fit14lme)

#fit14norandom <- lm(y~x6+x7,data=mydata,family=gaussian(link="log"))


#exactLRT(fit14lme)
#exactLRT(fit14lme,fit14norandom)
#?exactLRT





fit15 <- glmmPQL(y~x7,random=list(x2=~1),family=gaussian(link=log))
summary(fit15)
intervals(fit15,which="var-cov") #11:28 lesson 9 Part 2
fit15$coefficients
?glmmPQL

fit15bis <- glmmPQL(y~x6+x7,random=list(x2=~1+x6),family=gaussian(link=log))
summary(fit15bis)
intervals(fit15bis,which="var-cov") #11:28 lesson 9 Part 2
fit15bis$coefficients
?glmmPQL


library(hglm)







#QUESTION 4

#Part 4A

#ML
fit16 <- gam(y~x2+x3+x4+x5+x6+x7+x8+x9,data=mydata,family=gaussian(link="log"))
fit16 <- gam(PriceHouse~Province+Shops+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+Industries+HealthSocial,data=mydata,family=gaussian(link="log"))
#fit16check <- glm(y~x2+x3+x4+x5+x6+x7+x8+x9,data=mydata,family=gaussian(link="log"))

fit16check <- lme(y~x2+x3+x4+x5+x6+x7+x8+x9,data=mydata,method="ML")

summary(fit16)
summary(fit16)$coefficients

#summary(fit16check)

#(2) to (4)
library(h2o)
h2o.init()


mydat2=as.h2o(mydata)
mydat2$Region <- as.factor(mydat2$Region)
mydat2$Province <- as.factor(mydat2$Province)


y="PriceHouse"
X = c("Province", "Shops","Bankruptcies","MeanIncome","TaxForms","HotelRestaurant","Industries","HealthSocial")  # add here the variables that you wish to put in X.

#Ridge

alpha0  <- h2o.glm(family= "gaussian", link="log", x= X, y=y, alpha=0,
                   lambda_search=TRUE, training_frame=mydat2, nfolds=5)


h2o.coef(alpha0)
#Lasso

alpha1  <- h2o.glm(family= "gaussian", link="log", x= X, y=y, alpha=1,
                   lambda_search=TRUE, training_frame=mydat2, nfolds=5)


h2o.coef(alpha1)

#Elastic net

alpha0.5  <- h2o.glm(family= "gaussian", link="log", x= X, y=y, alpha=0.5,
                   lambda_search=TRUE, training_frame=mydat2, nfolds=5)


h2o.coef(alpha0.5)

#Part 4B

#(i)
Xevalgam1 <- as.data.frame(mydata[25,]) # I went for Hélécine
Xevalgam1
Xeval1 = as.h2o(as.data.frame(mydat2[25,]))
Xeval1
#ML
exp(predict(fit16,Xevalgam1))
#Ridge
h2o.predict(alpha0, newdata=Xeval1)
#Lasso
h2o.predict(alpha1, newdata=Xeval1)
#Elastic Net
h2o.predict(alpha0.5, newdata=Xeval1)

#(ii)
Xevalgam2 <- as.data.frame(mydata[c(127),])
Xevalgam2
Xeval2 = as.h2o(as.data.frame(mydat2[c(127),]))  # I went for Anderlecht
#ML
exp(predict(fit16,Xevalgam2))
#Ridge
h2o.predict(alpha0, newdata=Xeval2)
#Lasso
h2o.predict(alpha1, newdata=Xeval2)
#Elastic Net
h2o.predict(alpha0.5, newdata=Xeval2)

mydat2[25,]
mydat2[127,]



