install.packages("MuMIn")
install.packages("leaps")
library(MuMIn)
library(leaps)
library(MASS)


studentnumber = 123456
fulldata = read.table("dataHW.txt",header=T)
set.seed(studentnumber)
rownumbers = sample(1:nrow(fulldata),600,replace=F)
mydata = fulldata[rownumbers,]
attach(mydata)
head(mydata)

#Q1: Ik denk dat dit gewoon een poisson model moet zijn?

model <- glm(Y~X1+X2+X3+X4+X5, family = poisson(), data=mydata )
summary(model)
      
interacmodel <- glm(Y~.^2, family = poisson(), data = mydata )
summary(interacmodel)

quasimodel <- glm(Y~X1+X2+X3+X4+X5, family = quasipoisson() )
summary(quasimodel)

########################"#Q2



aicmydata <- stepAIC(model,k=2,direction="both",scope=list(upper~.^2,lower~1))
bicmydata <- stepAIC(model,k=log(nrow(mydata)),direction="both",scope=list(upper~.^2,lower~1))

interacaicmydata <- stepAIC(interacmodel,k=2,direction="both",scope=list(upper~.^2,lower~1))
interacbicmydata <- stepAIC(interacmodel,k=log(nrow(mydata)),direction="both",scope=list(upper~.^2,lower~1))


#check results on written down paper, difference between beginning with only main effects model vs full interaction model is small in the end in terms of AIC.
#I'm going to go for the simpler model though it's AIC is a bit higher


#backward gives the same results, forward doesn't do antything, not sure why not especially for the model case, 
#for the interacmodel it makes sense it can't add more terms

forwardaicmydata <- stepAIC(model,k=2,direction="forward",scope=list(upper~.^2,lower~1))
forwardbicmydata <- stepAIC(model,k=log(nrow(mydata)),direction="forward",scope=list(upper~.^2,lower~1))

forwardinteracaicmydata <- stepAIC(interacmodel,k=2,direction="forward",scope=list(upper~.^2,lower~1))
forwardinteracbicmydata <- stepAIC(interacmodel,k=log(nrow(mydata)),direction="forward",scope=list(upper~.^2,lower~1))


backwardaicmydata <- stepAIC(model,k=2,direction="backward",scope=list(upper~.^2,lower~1))
backwardbicmydata <- stepAIC(model,k=log(nrow(mydata)),direction="backward",scope=list(upper~.^2,lower~1))

backwardinteracaicmydata <- stepAIC(interacmodel,k=2,direction="backward",scope=list(upper~.^2,lower~1))
backwardinteracbicmydata <- stepAIC(interacmodel,k=log(nrow(mydata)),direction="backward",scope=list(upper~.^2,lower~1))



###########################Q3

#again:

aicmydata <- stepAIC(model,k=2,direction="both",scope=list(upper~.^2,lower~1))
interacaicmydata <- stepAIC(interacmodel,k=2,direction="both",scope=list(upper~.^2,lower~1))

bicmydata <- stepAIC(model,k=log(nrow(mydata)),direction="both",scope=list(upper~.^2,lower~1))

#so for AIC: 
#best model is with X1, x2, X3, X5,
#second is X1, X2 & X5
#third is X1, x2, X3, X4 & X5






###########################Q5 FIC
detach(mydata)
attach(mydata)
install.packages("fic")
library(fic)

#wide model
wide.mydata <- glm(Y~., data=mydata, family=poisson())

#narrow model
inds0 <- c(1,rep(0,length(wide.mydata$coefficients)-1))

#focus function
focus <- function(par, X) exp(X %*% par)


meanvalue <- c(1,mean(X1),mean(X2),mean(X3),mean(X4),mean(X5))
meanvalue

maxvalue <- c(1,max(X1),max(X2),max(X3),max(X4),max(X5))
maxvalue

X.eval <- rbind(meanvalue, maxvalue)


combs <- all_inds(wide.mydata, inds0)
ficres <- fic(wide=wide.mydata, inds=combs, inds0=inds0, focus=focus, X=X.eval)

#there's a problem with singularity, still I see only correlation between X3 en X5 is significant with almost 0.6?
cor.test(X1,X2)
cor.test(X1,X3)
cor.test(X1,X4)
cor.test(X1,X5)
cor.test(X2,X3)
cor.test(X2,X4)
cor.test(X2,X5)
cor.test(X3,X4)
cor.test(X3,X5)
cor.test(X4,X5)


#rest van FIC


summary(ficres,adj=TRUE)


ficmean = ficres[ficres$vals=="meanvalue",]
inds <- attr(ficmean, "inds")
best3 = order(ficmean$rmse.adj)[1:3]
ficmean[best3,]

ficmax = ficres[ficres$vals=="maxvalue",]
inds <- attr(ficmax, "inds")
best3 = order(ficmax$rmse.adj)[1:3]
ficmax[best3,]



