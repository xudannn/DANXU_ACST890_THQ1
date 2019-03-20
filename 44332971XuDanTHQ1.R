#Question1
Bond <-function(Coupon,Face,n,Yields){
  Y <- c(Yields)
  price <- 0
  PVF <- Face*exp(-n*Y[n])
  for (n in 1:n) {
    CF = Coupon*exp(-n*Y[n])
    price <- price + CF
   }
  return(price+PVF)
}

#In the console, we can type 
Bond(20,1000,4,c(0.01,0.02,0.03,0.04))

#Question3
# (a)
dataset <- read.csv(file.choose(), header=T)

# (b)
dataset  <- na.omit(dataset )

# (c)
plot(dataset$time,dataset$gdp,main= "Singapore GDP growth", xlab="Time", ylab="GDP(%)")

# (d)
group_mean <- tapply(dataset$gdp, dataset$period, mean)
group_sd <- tapply(dataset$gdp, dataset$period, sd)
period <- c(1,2,3)
stat.table <- data.frame(period,group_mean,group_sd)
stat.table

# (e)
library(dplyr)
pairs(select(dataset,-time,-period))

# (f)
lm.fit1 = lm(dataset$gdp ~ dataset$exp)
summary(lm.fit1)

# (g)
lm.fit2=lm(dataset$gdp~dataset$exp + dataset$epg + dataset$hpr + dataset$oil + dataset$gdpus + dataset$crd)
summary(lm.fit2)

# (h)
quan <- quantile(dataset$gdp,0.05)
quan

state <- ifelse(dataset$gdp<quan,"crisis","normal")
newdataset <- data.frame(dataset, state)
tr <- subset(newdataset, newdataset$period==1|newdataset$period==2)
te <- subset(newdataset, newdataset$period==3)
logit<- glm(state~bci, data = tr, family = binomial)
logit

prediction <- predict(logit, te, type = "response")
fittedglm <- rep("crisis", 38)
fittedglm[prediction>0.5]="normal"
table(fittedglm, te$state)
