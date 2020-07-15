library(expm)
library(ggplot2)
library(plyr)
library(reshape2)
library(matrixStats)
set.seed(5)

#https://stats.stackexchange.com/questions/254106/understanding-hidden-markov-model-hmm
#Probability of being picked after n draws - sample with replacement, 1 out of 100 balls in basket
Trns <- matrix(c(0.99, 0.01, 0, 1), nrow = 2, byrow = T, dimnames = list(c("b","d"),c("B","D")))
Trns
State <- c(1,0)
State

State%*%Trns
State%*%Trns%*%Trns
State%*%Trns%*%Trns%*%Trns

State%*%(Trns%^%100)

1-pgeom(q = 99,prob = 0.01)
#how do we do this if there's sample without replacement?
#distribution is not stationary!
#__________________________________________________________


#______________________________________________________
#Assume you are a health fund, loooking at modelling the risk of members against a particular disease.
#Patients can be in the "healthy" group, or the "at-risk" group. Either group can have an "event" which can be fatal
#or lead to recovery.

#state transition
HealthTrns <- matrix(c(0.90, 0.05, 0.05, 0.0,
                       0.00, 0.00, 0.50, 0.5,
                       0.25, 0.25, 0.5, 0.0,
                       0.00, 0.00, 0.00, 1.0),
                       nrow = 4, byrow = T,
                     dimnames = list(c("healthy", "event", "risk", "death"),
                                     c("Healthy", "Event", "Risk", "Death")))

#probabilities of outcomes for healthy person in x years
c(1,0,0,0)%*%(HealthTrns%^%40)
#probabilities of outcomes for "healthy"at risk" person in x years
c(0,0,1,0)%*%(HealthTrns%^%40)

HealthState <- c(1,0,0,0)
probs <- cbind(HealthState,sapply(X = 1:40, function(x) HealthState%*%(HealthTrns%^%x)))
DFprobs <- as.data.frame(cbind(0:40,t(probs)))
names(DFprobs) <-c("years","healthy", "event", "risk", "death")
DFprobs <- melt(DFprobs, id.vars = "years")
ggplot(DFprobs, aes(x=years, y=value, fill = variable))+geom_area()+ggtitle("probability of health outcome")



#a health plan financial risk simulation with MCMC
cf <- data.frame(Healthy=c(1000), Event=c(-10000), Risk=c(1000), Death=c(0))

#Random process: decide the next state of the patients
processRow <- function(patientState){
  sapply(patientState, function(x)  sample(x=c(1,2,3,4), size=1, replace=TRUE, prob= HealthTrns[x,]))
}
#initial state: everyone is healthy
patientStates <- matrix(rep(1,10000))
#run the process over 40 years
for(i in 1:40){patientStates <- cbind(patientStates,processRow(patientStates[,i]))}

#rearrange array and plot a sample of member's chonology of the disease
DF <- as.data.frame(cbind(1:41,t(patientStates)))
names(DF) <- c('yr', paste("patient",1:(ncol(DF)-1),sep = '_'))
ggDF <- melt(DF[sample(1:nrow(DF), 30)], id.vars = 'yr')
ggDF$value <- factor(ggDF$value, labels = c("healthy", "event", "risk", "death"))
ggplot(ggDF, aes(x=yr, y=variable, fill = value))+geom_tile()+ggtitle("examples of member chronology")

#replace the states by their associated costs
financials <- apply(patientStates, 1:2, function(x) cf[1,x])

rowSums(financials) #revenue from each individual
max(rowSums(financials))
min(rowSums(financials))
table(patientStates[which.min(rowSums(financials)),])

cumFinancials <- rowCumsums(financials)

DFtraces <- data.frame(t(cumFinancials[sample(1:dim(cumFinancials)[1], 200),]))
DFtraces$year <- 1:41
DFtraces <- melt(DFtraces, id.vars='year')
ggplot(DFtraces, aes(x=year,y=value,group=variable))+geom_line(alpha = 0.2,position=position_jitter(w=0, h=2000))

DFsums <- as.data.frame(t(apply(cumFinancials,2,function(x) c(summary(x), sd=sd(x), IQR=IQR(x)))))
DFsums$year <- 1:41
ggplot(DFsums, aes(x=year,y=Median))+geom_line()+
  geom_line(aes(y=`Mean`), colour = 'purple')+
  geom_line(aes(y=`1st Qu.`), colour = 'red')+
  geom_line(aes(y=`3rd Qu.`), colour = 'blue')+
  geom_ribbon(aes(ymin=`Min.`,ymax=`Max.`), fill = 'black', alpha = 0.2)+
  ylab("revenue")+
  xlab("years")+
  ylim(-75000,50000)


#Cost
DFsums$Mean[41]
#mortality at end of study
length(which(patientStates[,41] == 4))

#______________________________
#comparing with an intervention
set.seed(5)
HealthTrns2 <- matrix(c(0.90, 0.05, 0.05, 0.0,
                        0.00, 0.00, 0.50, 0.5,
                        0.50, 0.25, 0.25, 0.0,
                        0.00, 0.00, 0.00, 1.0),
                     nrow = 4, byrow = T,
                     dimnames = list(c("healthy", "event", "risk", "death"),
                                     c("Healthy", "Event", "Risk", "Death")))

cf2 <- data.frame(Healthy=c(1000), Event=c(-10000), Risk=c(-200), Death=c(0))

HealthState <- c(1,0,0,0)
probs2 <- cbind(HealthState,sapply(X = 1:40, function(x) HealthState%*%(HealthTrns2%^%x)))
DFprobs2 <- as.data.frame(cbind(0:40,t(probs2)))
names(DFprobs2) <-c("years","healthy", "event", "risk", "death")
DFprobs2 <- melt(DFprobs2, id.vars = "years")
DFprobs2$intervention <- TRUE
DFprobs$intervention <- FALSE
DFall <- rbind(DFprobs2,DFprobs)
ggplot(DFall, aes(x=years, y=value, fill = variable))+
  geom_area(alpha = 0.6)+
  ggtitle("probability of health outcome")+
  facet_wrap(~intervention)

processRow2 <- function(patientState){
  sapply(patientState, function(x)  sample(x=c(1,2,3,4), size=1, replace=TRUE, prob= HealthTrns2[x,]))
}
patientStates2 <- matrix(rep(1,10000))
for(i in 1:40){patientStates2 <- cbind(patientStates2,processRow2(patientStates2[,i]))}
financials2 <- apply(patientStates2, 1:2, function(x) cf2[1,x])
cumFinancials2 <- rowCumsums(financials2)
DFsums2 <- as.data.frame(t(apply(cumFinancials2,2,function(x) c(summary(x), sd=sd(x), IQR=IQR(x)))))
DFsums2$year <- 1:41
DFsums2$intervention <- TRUE
DFsums$intervention <- FALSE
DFsumsAll <- rbind(DFsums,DFsums2)
ggplot(DFsumsAll, aes(x=year,y=Median))+geom_line()+
  geom_line(aes(y=`Mean`), colour = 'purple')+
  geom_line(aes(y=`1st Qu.`), colour = 'red')+
  geom_line(aes(y=`3rd Qu.`), colour = 'blue')+
  geom_ribbon(aes(ymin=`Min.`,ymax=`Max.`), fill = 'black', alpha = 0.2)+
  ylab("revenue")+
  xlab("years")+
  ylim(-75000,50000)+
  facet_wrap(~intervention)


#Cost
c(DFsums$Mean[41],DFsums2$Mean[41])
#mortality at end of study
matrix(c(table(patientStates[,41]),table(patientStates2[,41])),nrow =2, byrow = T)/10000
#test of statistical significance
prop.test(c(sum(patientStates[,41]==4), sum(patientStates2[,41]==4)),c(10000,10000))
#What if we only ran our experiment on 200 members, instead of 10,000
prop.test(c(sum(patientStates[,41]==4), sum(patientStates2[,41]==4))/50 ,c(10000,10000)/50)
