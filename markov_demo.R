library(expm)
library(ggplot2)
library(plyr)
library(reshape2)
library(matrixStats)
#Probability of being picked after n draws - sample with replacement, 1 out of 100 balls in urn
Trns <- matrix(c(0.99, 0.01, 1, 0), nrow = 2, byrow = T, dimnames = list(c("u","d"),c("U","D")))
State <- c(1,0)

State%*%(Trns^100)

1-pgeom(q = 99,prob = 0.01)
#how do we do this if there's sample without replacement?
#distribution is not stationary


#state probabilities forecasting
HealthTrns <- matrix(c(0.90, 0.05, 0.05, 0.0,
                       0.00, 0.00, 0.50, 0.5,
                       0.35, 0.25, 0.4, 0.0,
                       0.00, 0.00, 0.00, 1.0),
                       nrow = 4, byrow = T,
                     dimnames = list(c("healthy", "event", "risk", "death"),
                                     c("Healthy", "Event", "Risk", "Death")))
HealthState <- c(1,0,0,0)

HealthState%*%(HealthTrns%^%40)
probs <- sapply(X = 1:40, function(x) HealthState%*%(HealthTrns%^%x))
DF <- as.data.frame(cbind(1:40,t(probs)))
names(DF) <-c("years","healthy", "event", "risk", "death")
DF <- melt(DF, id.vars = "years")
#ggplot(DF, aes(x=years, y=value, colour = variable))+geom_line()
#ggplot(DF, aes(x=years, y=value, fill = variable))+geom_area()



#a health plan simulation with MCMC
cf <- data.frame(Healthy=c(1000,1000), Event=c(-10000,-10000), Risk=c(1000,1000), Death=c(0,0))

processRow <- function(patientState){
  sapply(patientState, function(x)  sample(x=c(1,2,3,4), size=1, replace=TRUE, prob= HealthTrns[x,]))
}

patientStates <- matrix(rep(1,10000))
for(i in 1:40){patientStates <- cbind(patientStates,processRow(patientStates[,i]))}
#DF <- as.data.frame(cbind(1:41,t(patientStates)))
#names(DF) <- c('yr', paste("patient",1:50000,sep = '_'))
#ggDF <- melt(DF, id.vars = 'yr')
#ggplot(ggDF, aes(x=yr, y=variable, fill = as.factor(value)))+geom_tile()

financials <- apply(patientStates, 1:2, function(x) cf[1,x])

rowSums(financials) #revenue from each individual
which.max(rowSums(financials))
which.min(rowSums(financials))
table(patientStates[which.min(rowSums(financials)),])

cumFinancials <- rowCumsums(financials)


DF <- as.data.frame(t(apply(cumFinancials,2,function(x) c(summary(x), sd=sd(x), IQR=IQR(x)))))
DF$year <- 1:41
ggplot(DF, aes(x=year,y=Median))+geom_line()+
  geom_line(aes(y=`Mean`), colour = 'purple')+
  geom_line(aes(y=`1st Qu.`), colour = 'red')+
  geom_line(aes(y=`3rd Qu.`), colour = 'blue')+
  geom_ribbon(aes(ymin=`Min.`,ymax=`Max.`), fill = 'black', alpha = 0.2)


