#loading in relevant packages (dependencies)
library(pscl)
library(psc)
library(ggplot2)
library(dplyr)
library(MASS)




dat=read.csv("./THESIS.csv") #read in original thesis data (no off effort events included)
names(dat)=make.names(names(dat)) #creates valid variable names
source("./ExpandDF_Function.R") #routines that unpack the multi-item rows for SH and SF
dat.update<-read.csv("./THESIS.update.csv") #data file that includes on and off effort SFE


########################## Preliminary data/cleaning up the datasheet

dat$SF_Zone=gsub(" ", "", dat$SF_Zone) #taking out extra spaces from the dataset


#formatting the dates as dates
dat$Start.time = as.POSIXct(paste(dat$Date,dat$Start.time),format="%m/%d/%Y %H:%M")
dat$End.time = as.POSIXct(paste(dat$Date,dat$End.time),format="%m/%d/%Y %H:%M")
dat$Low.tide.time = as.POSIXct(paste(dat$Date,dat$Low.tide.time),format="%m/%d/%Y %H:%M")
dat$Date = as.Date(dat$Date,format="%m/%d/%Y") 
#same as above, but for off-effort dataset:
dat.update$Start.time = as.POSIXct(paste(dat.update$Date,dat.update$Start.time),format="%m/%d/%Y %H:%M")
dat.update$End.time = as.POSIXct(paste(dat.update$Date,dat.update$End.time),format="%m/%d/%Y %H:%M")
dat.update$Low.tide.time = as.POSIXct(paste(dat.update$Date,dat.update$Low.tide.time),format="%m/%d/%Y %H:%M")
dat.update$Date = as.Date(dat.update$Date,format="%m/%d/%Y")



dat$diff.from.low.time = dat$Start.time - dat$Low.tide.time #creating a difference from low tide time column
#historgram for number of strand feeding events at each hour from low tide
hist(as.numeric(dat$diff.from.low.time)/3600, 
     ylab = "Number of SF Events",
     xlab="Difference in hours from time of low tide")

#Same thing (creating difference from low tide time column)
#but for datasheet that includes off-effort data
diff.from.low.time.2 <- newdat.update$Start.time - newdat.update$Low.tide.time


#creating ZRatios (proportion of people in each zone in front of the distance)
tmpdf=dat[,names(dat)[grep("in.front.AVG",names(dat))]]/(dat[,names(dat)[grep("behind.AVG",names(dat))]]+dat[,names(dat)[grep("in.front.AVG",names(dat))]])
names(tmpdf)=c("Z1Ratio","Z3Ratio","Z5Ratio")
dat=cbind(dat,tmpdf)
dat$AvgZRatio = rowMeans(dat[,c("Z1Ratio","Z3Ratio","Z5Ratio")],na.rm=T)

#Same as above, but includes off-effort stranding events:
tmpdf=dat.update[,names(dat.update)[grep("in.front.AVG",names(dat.update))]]/(dat.update[,names(dat.update)[grep("behind.AVG",names(dat.update))]]+dat.update[,names(dat.update)[grep("in.front.AVG",names(dat.update))]])
names(tmpdf)=c("Z1Ratio","Z3Ratio","Z5Ratio")
dat.update=cbind(dat.update,tmpdf)
dat.update$AvgZRatio = rowMeans(dat.update[,c("Z1Ratio","Z3Ratio","Z5Ratio")],na.rm=T)



#glm to test how ZRatio changes with distance tested
fit.comp=glm((AvgZRatio>0)~Distance.tested, data=dat, family=binomial)
summary(fit.comp)
#prediction plot using jitter (as distance increases, noncompliance increases)
plot((AvgZRatio>0)~jitter(Distance.tested), data=dat) #creates plot for predict line
pdat=data.frame(Distance.tested=0:100) #new df for predict line
points(predict(fit.comp, newdata = pdat, type="response")~pdat$Distance.tested, type="l")


#new dataframe to expand the columns where dolphins strand fed in multiple zones
#uses the expand dataframe function created in separate file
newdat=expandDF(dat,cols="SF_Zone") #splits up SF zone by ";"


#Using function expandDF again to expand strand feeding zone and strand feeding individual
newdat=expandDF(dat,cols=c("SF_Zone","SF_Ind"))
newdat = newdat %>% group_by(SF_Zone) %>% summarise(SF_Ind=sum(as.numeric(SF_Ind),na.rm=T))


#same as above, but including off-effort data:
newdat.update=expandDF(dat.update,cols=c("SF_Zone", "SF_Ind"))
newdat.update$SF_Ind=as.numeric(gsub(" ", "", newdat.update$SF_Ind))
#barplot to show SFE per zone:
barplot(table(as.numeric(newdat.update$SF_Zone)),xlab="Zone",ylab="Count of strand feeding events")



#classifying number of strand feeding individuals and strand feeding zone as numeric
newdat$SF_Ind = as.numeric(newdat$SF_Ind)
newdat$SF_Zone = as.numeric(newdat$SF_Zone)


#jitter plot to represent the number of strand feeding events at each distance, 
#point size represents compliance (bigger = more compliant)
plot(jitter(Number.Sf.Events)~jitter(Distance.tested),cex=log(1/(AvgZRatio+.1)),data=dat,
     ylab = "Number SF Events", xlab = "Distance Tested")
legend("topright",legend=c("Point size represents compliance"))
abline(coef(lm(dat$Number.Sf.Events~dat$Distance.tested+dat$diff.from.low.time))[1:2])

#same plot BUT includes off-effort strand feeding events:
plot(jitter(Number.Sf.Events)~jitter(Distance.tested),cex=log(1/(AvgZRatio+.1)),data=dat.update,
     ylab = "Number SF Events", xlab = "Distance Tested")
legend("topright",legend=c("Point size represents compliance"))
abline(coef(lm(dat.update$Number.Sf.Events~dat.update$Distance.tested))[1:2])


#jitter plot to represent how non-compliance increases with distance tested
plot(jitter(AvgZRatio)~ jitter(Distance.tested),data=dat,ylab="Non-compliance score",
     xlab="Distance treatment (ft)")
abline(coef(lm(AvgZRatio~ Distance.tested,data=dat))[1:2]) 


#creating a subset of data (Islands) to group zones to reflect which island they are on
#(1, 3 and 5 = Kiawah Island), (2 and 4 = Seabrook Island)
newdat.update$SF_Zone[grepl("SBI", newdat.update$SF_Zone)]=6
newdat.update$Island<-c("KI","SBI","KI","SBI","KI", "SBI")[as.numeric(newdat.update$SF_Zone)]
table(newdat.update$Island, gsub(" ", "", newdat.update$SF_Zone)) #getting rid of unwanted spaces 


#Function to only look at strand feeding events if proportion of people in front of 
#the distance is < 10%
if(truncateZ)
{dat=dat[dat$AvgZRatio<0.1,]}
#all data going forward will have a compliance score of 90% and above




################################## ANALYSIS

#####Objective 1: how does distance treatment affect disturbance behaviors?

#Combining disturbance behaviors (tail slapping and chuffs) into one level
newdat.update$disturb <- newdat.update$Number_TS + newdat.update$Number_Chuff


#Linear model to test how disturbance is affected by distance treatment
obj1.mod <- lm(disturb[newdat.update$disturb>0] ~ Distance.tested[newdat.update$disturb>0], 
               data=newdat.update)
summary(obj1.mod)
AIC(obj1.mod)

#Linear model with more variables included to test how they might interact with
#number of SFE:
obj1.full.lm<-lm(disturb[newdat.update$disturb>0] ~ Distance.tested[newdat.update$disturb>0]
                             + KI.AVG.total[newdat.update$disturb>0] + SBI.total.AVG[newdat.update$disturb>0],
                             data=newdat.update)
summary(obj1.full.lm)
AIC(obj1.full.lm)

#Jitter plot representing linear model
plot(jitter(newdat.update$disturb[newdat.update$disturb>0]) ~ 
       jitter(newdat.update$Distance.tested[newdat.update$disturb>0]), 
     xlab="Distance Tested", ylab="Disturbance Behaviors Displayed")
abline(obj1.mod, col = "red", lwd = 2, lty = 2)

#examining residuals for objective 1 lm
plot(obj1.mod)


#Poisson glm for disturbance behaviors by distance treatment
#First for only disturbance > 0:
obj1.pois<-glm(disturb[newdat.update$disturb>0] ~ Distance.tested[newdat.update$disturb>0]
               + KI.AVG.total[newdat.update$disturb>0] + SBI.total.AVG[newdat.update$disturb>0],
               data=newdat.update, family = "poisson")
summary(obj1.pois)
AIC(obj1.pois)
#next for all 5-minute periods:
obj1pois2<-glm(disturb~Distance.tested
               + KI.AVG.total + SBI.total.AVG,
               data=newdat.update, family = "poisson")
summary(obj1pois2)
pR2(obj1pois2)
#examining residuals for poisson objective 1 model:
plot(obj1.pois)

#gaussian glm for disturbance behaviors by distance treatment
obj1.gaus<-glm(disturb ~ Distance.tested
               + KI.AVG.total + SBI.total.AVG,
               data=newdat.update, family = "gaussian")
summary(obj1.gaus)
AIC(obj1.gaus)
pR2(obj1.gaus)
               

#Zero inflated model for disturbance (results are roughly the same as the lm because we ran
#it with disturb > 0):
zim_pois_obj1 <- zeroinfl(disturb ~ Distance.tested + KI.AVG.total + 
                            SBI.total.AVG, 
                          data = newdat.update, 
                          dist = "poisson")
summary(zim_pois_obj1)
AIC(zim_pois_obj1)
pR2(zim_pois_obj1)


#comparing models using a plot to understand which has the best fit (only IV is distance tested)
disturb_KI_pois<-glm(disturb~Distance.tested, data=newdat.update, family = "poisson")
disturb_KI_gau<-glm(disturb~Distance.tested, data=newdat.update, family = "gaussian")
disturb_KI_zim<-zeroinfl(disturb~Distance.tested | Distance.tested, data=newdat.update)

summary(disturb_KI_zim)
AIC(disturb_KI_zim)
pR2(disturb_KI_zim)

#model comparison plot:
dist_newdata<-data.frame(Distance.tested = seq(min(newdat.update$Distance.tested), max(newdat.update$Distance.tested), length.out = 100))
plot(jitter(disturb)~Distance.tested, data=newdat.update, xlab="Distance Treatment (ft)",
     ylab="Count of Disturbance Behaviors Displayed")
lines(dist_newdata$Distance.tested,
      predict(disturb_KI_gau, newdata=dist_newdata), col="red", lwd=2)
lines(dist_newdata$Distance.tested,
      predict(disturb_KI_pois, newdata=dist_newdata, type="link"),
      col="purple3", lwd=2)
lines(dist_newdata$Distance.tested, 
      predict(disturb_KI_zim, newdata=dist_newdata, type="count"),
      col="blue", lwd=2)
legend("topright",   
       legend = c("Gaussian", "Poisson", "ZINB"),  
       col = c("red", "purple3", "blue"),
       lwd = 2,
       bty = "n")  # removes the box around the legend 

#ZIM had the best fit overall

#####Objective 2: How does distance treatment affect how likely dolphins are to strand feed?

plot(jitter(newdat.update$Number.Sf.Events[newdat.update$Number.Sf.Events>0])~
       jitter(newdat.update$Distance.tested[newdat.update$Number.Sf.Events>0]), xlab= "Distance Tested",
              ylab="Number SFE")

#GLM representing how number of SFE is affected by distance treatment
obj2.mod<-glm(Number.Sf.Events ~ Distance.tested + KI.AVG.total + diff.from.low.time.2, data = newdat.update)
summary(obj2.mod)
pR2(obj2.mod)

#poisson model:

obj2.pois<-glm(Number.Sf.Events ~ Distance.tested + KI.AVG.total +  
                 diff.from.low.time.2, data=newdat.update, family = "poisson")
summary(obj2.pois)
AIC(obj2.pois)
pR2(obj2.pois) #Pseudo R2 for poisson glm

#Zero inflated model for number of SF events
zim_obj2_SFE<-zeroinfl(Number.Sf.Events~Distance.tested + KI.AVG.total + diff.from.low.time.2,
                       data=newdat.update, dist="poisson")
summary(zim_obj2_SFE)
AIC(zim_obj2_SFE)
pR2(zim_obj2_SFE)

#zim has a higher R2 value, but poisson has a higher AIC
#Both show difference from low tide time affects the number of strand feeding events

##Obj 2 model comparison (Number SFE is DV, distance tested is only IV)
SFE_pois<-glm(Number.Sf.Events~Distance.tested, data=newdat.update, family = "poisson")
SFE_gau<-glm(Number.Sf.Events~Distance.tested, data=newdat.update, family = "gaussian")
SFE_zim<-zeroinfl(Number.Sf.Events~Distance.tested | Distance.tested, data=newdat.update)

summary(SFE_zim)
AIC(SFE_zim)

#model comparison plot
SFE_newdat<-data.frame(Distance.tested = seq(min(newdat.update$Distance.tested), max(newdat.update$Distance.tested), length.out = 100))
plot(jitter(Number.Sf.Events)~Distance.tested, data=newdat.update, xlab="Distance Treatment (ft)",
     ylab="Count of SFE")
lines(SFE_newdat$Distance.tested,
      predict(SFE_gau, newdata=SFE_newdat), col="red", lwd=2)
lines(SFE_newdat$Distance.tested,
      predict(SFE_pois, newdata=SFE_newdat, type="link"),
      col="purple3", lwd=2)
lines(SFE_newdat$Distance.tested, 
      predict(SFE_zim, newdata=SFE_newdat, type="count"),
      col="blue", lwd=2)
legend("topright",   
       legend = c("Gaussian", "Poisson", "ZINB"), 
       col = c("red", "purple3", "blue"),
       lwd = 2,
       bty = "n")  # removes the box around the legend 




#Binary model (logistic) 
#First, I made strand feeding a binary rather than a count:
newdat.update$SFE.binary <- ifelse(as.numeric(as.character(newdat.update$Number.Sf.Events)) > 0, 1, 0)
#Logistic regression with all factors included:
obj2.logit<-glm(SFE.binary ~ Distance.tested + KI.AVG.total +  
                  diff.from.low.time.2, data=newdat.update, family = "binomial")
summary(obj2.logit)
pR2(obj2.logit) 

#Logistic regression with just the factor I'm interested in (distrance treatment) in order
#to make the predict plot:
obj2.logit.distance<-glm(SFE.binary ~ Distance.tested, data=newdat.update, family = "binomial")
summary(obj2.logit.distance)

#Predict plot for whether dolphins will feed based on distqnce tested:
SFEBin=data.frame(Distance.tested=0:80)
predict(obj2.logit.distance, newdata=SFEBin, type="response")
plot(predict(obj2.logit.distance,newdata=SFEBin,type="response")~I(0:80),type="l",ylim=c(0,1), col="black",
     xlab="Distance Treatment", ylab="Probability of Dolphin Strand Feeding")
points(I(1-predict(obj2.logit.distance,newdata=SFEBin,type="response"))~I(0:80),type="l",col="red")
legend("topright", legend = c("Yes", "No"), col = c("black", "red"), lty=1, cex=0.8)




####Objective 3: Does the number of people affect where strand feeding occurs?

#First, need to specify Kiawah and Seabrook as the binaries:
KIyesno = ifelse(newdat.update$Island == "KI", 1, 0)

#Logistic regression model to test how relavent factors (dostance tested, number of 
#people on KI and SBI) affect where dolphins feed:
obj3.logit<-glm(KIyesno ~ Distance.tested + KI.AVG.total + SBI.total.AVG, 
                data=newdat.update, family="binomial")
summary(obj3.logit) #Number of people on KI inversely affects dolphins strand feeding on KI
pR2(obj3.logit)


#plot representing how probability of strand feeding on KI and SBI is affected by the number 
#of people on KI
obj4<-glm(KIyesno ~ KI.AVG.total, 
         data=newdat.update, family="binomial")
summary(obj4)
KIPop=data.frame(KI.AVG.total=2:50)
predict(obj4,newdata=KIPop,type="response")
plot(predict(obj4,newdata=KIPop,type="response")~I(2:50),type="l",ylim=c(0,1), col="black",
     xlab="Number of People on Kiawah", ylab="Probability of Dolphin Strand Feeding")
points(I(1-predict(obj4,newdata=KIPop,type="response"))~I(2:50),type="l",col="red")
legend("topright", legend = c("Kiawah", "Seabrook"), col = c("black", "red"), lty=1, cex=0.8)

