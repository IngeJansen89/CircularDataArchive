#Empty global environment
rm(list=ls())

#Load/source required packages/files
require(circular)
require(foreign)
require(MASS)

################################################################################################
#######################Data Manipulation########################################################
################################################################################################

#Read data
germany <- read.dta('Raw data/germandata.dta')

#Convert counterclockwise angles (as in code Gill & Hangartner)
germany$theta <- (360-germany$direction) / 360 * 2* pi

#Compute variables for analysis (as in code Gill & Hangartner)

#Year of the election and Year of election^2 to control for changes in status
#quo between elections
germany$Year   <- germany$year-min(germany$year)
germany$Yearsq <- (germany$Year^2)/100

#Further computations as in Gill & Hangartner
germany$unemp100      <- germany$unemp/100
germany$outofwed2.100 <- germany$outofwed2/100


#Make new dataset (as in code Gill & Hangartner)

GERMANY <- cbind(germany$theta, germany$unemp100, germany$outofwed2.100, germany$reunification, germany$spd, 
                 germany$cducsu, germany$green, germany$pds, germany$Year, germany$Yearsq)

#summary of theta (overall and per party) for Table 4:
(summary(circular(germany$theta*360/(2*pi),units="degrees"))+360)%%(360)
(summary(circular(germany$theta[which(germany$fdp==1)]*360/(2*pi),units="degrees"))+360)%%(360)
(summary(circular(germany$theta[which(germany$spd==1)]*360/(2*pi),units="degrees"))+360)%%(360)
(summary(circular(germany$theta[which(germany$cducsu==1)]*360/(2*pi),units="degrees"))+360)%%(360)
(summary(circular(germany$theta[which(germany$green==1)]*360/(2*pi),units="degrees"))+360)%%(360)
(summary(circular(germany$theta[which(germany$pds==1)]*360/(2*pi),units="degrees"))+360)%%(360)

#Prepare data for analysis
GERMANY <- as.data.frame(GERMANY)
names(GERMANY) <- c("direction", "unemp", "outofwed", "reunification", "spd", "cducsu", "green","pds","year","yearsq")

#Correlations between continuous variables:
cor(data.frame(GERMANY$unemp,GERMANY$outofwed,GERMANY$year,GERMANY$yearsq))

#Summary statistics for table 5:
summary(GERMANY)
mean(GERMANY$reunification)
mean(GERMANY$unemp)
mean(GERMANY$outofwed)
mean(GERMANY$year)
mean(GERMANY$yearsq)

sd(GERMANY$reunification)
sd(GERMANY$unemp)
sd(GERMANY$outofwed)
sd(GERMANY$year)
sd(GERMANY$yearsq)

###############################################################################################
#######################################Full Model##############################################
###############################################################################################

#Export data for full model
write.table(GERMANY, row.names=F, col.names=F,file="germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0.dat") 

#Info on analysis:
#This and all other files generated below can be analysed using the following procedure. 
#Ccodes and info on compiling are available in the folder:../2 Simulation study/Ccodes_and_scripts.
#
#First, use the regression code:
#
# ./Regression_<WN/WC>.exe 1 5 germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0.dat
#
#Then, use the Analysis code on the resulting output file:
#
# ./Analysis.exe 1 output_foranalysis_5000_1_germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0.dat
#
#Note that outputfiles are not included in the research archive in order to save space.

################################Import sampling results WN###################################################
autocor <- read.table("Full model/autocor_5000_1_germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","b3","b4","b5","b6","b7","b8","b9","rho")

plot(autocor$b0[1:500]~autocor$lag[1:500], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b1[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b1[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b2[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b2[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b3[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b3[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b4[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b4[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b5[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b5[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b6[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b6[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b7[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b7[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b8[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b8[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b9[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b9[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$rho[1:500]~autocor$lag[1:500], col="red", type="l",ylim=c(0,2))
lines(autocor$rho[501:1000]~autocor$lag[501:1000], col="blue")


##################Trace plots of data for all variables#############################
raw <- read.table("Full model/raw_output_20000_1_germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat", header=F)
names(raw) <- c("i","b0a","b0b","b1a","b1b","b2a", "b2b","b3a","b3b","b4a","b4b","b5a","b5b","b6a","b6b","b7a","b7b","b8a","b8b","b9a","b9b","ra","rb")

plot(raw$b0a, type="l", col="red")
lines(raw$b0b,  col="blue")
plot(raw$b1a, type="l", col="red")
lines(raw$b1b,  col="blue")
plot(raw$b2a, type="l", col="red")
lines(raw$b2b,  col="blue")
plot((raw$b3a+pi)%%(2*pi), type="l", col="red")
lines((raw$b3b+pi)%%(2*pi),  col="blue")
plot(raw$b4a, type="l", col="red")
lines(raw$b4b,  col="blue")
plot(raw$b5a, type="l", col="red")
lines(raw$b5b,  col="blue")
plot(raw$b6a, type="l", col="red")
lines(raw$b6b,  col="blue")
plot(raw$b7a, type="l", col="red")
lines(raw$b7b,  col="blue")
plot(raw$b8a, type="l", col="red")
lines(raw$b8b,  col="blue")
plot(raw$b9a, type="l", col="red")
lines(raw$b9b,  col="blue")
plot(raw$ra, type="l", col="red")
lines(raw$rb,  col="blue")



################################Import sampling results WC###################################################
autocor <- read.table("Full model/autocor_5000_1_germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","b3","b4","b5","b6","b7","b8","b9","rho")


plot(autocor$b0[1:500]~autocor$lag[1:500], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b1[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b1[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b2[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b2[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b3[1:500]~autocor$lag[1:500], col="red", type="l", ylim=c(0,20))
lines(autocor$b3[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b4[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b4[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b5[1:500]~autocor$lag[1:500], col="red", type="l", ylim=c(0,20))
lines(autocor$b5[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b6[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b6[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b7[1:500]~autocor$lag[1:500], col="red", type="l", ylim=c(0,20))
lines(autocor$b7[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b8[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b8[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$b9[1:500]~autocor$lag[1:500], col="red", type="l")
lines(autocor$b9[501:1000]~autocor$lag[501:1000], col="blue")
plot(autocor$rho[1:500]~autocor$lag[1:500], col="red", type="l",ylim=c(0,2))
lines(autocor$rho[501:1000]~autocor$lag[501:1000], col="blue")

##################Trace plots of data for all variables#############################
raw <- read.table("Full model/raw_output_20000_1_germanyfull_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat", header=F)
names(raw) <- c("i","b0a","b0b","b1a","b1b","b2a", "b2b","b3a","b3b","b4a","b4b","b5a","b5b","b6a","b6b","b7a","b7b","b8a","b8b","b9a","b9b","ra","rb")


plot(raw$b1a, type="l", col="red")
lines(raw$b1b,  col="blue")
plot(raw$b2a, type="l", col="red")
lines(raw$b2b,  col="blue")
plot((raw$b3a+pi)%%(2*pi), type="l", col="red")
lines((raw$b3b+pi)%%(2*pi),  col="blue")
plot(raw$b4a, type="l", col="red")
lines(raw$b4b,  col="blue")
plot(raw$b5a, type="l", col="red")
lines(raw$b5b,  col="blue")
plot(raw$b6a, type="l", col="red")
lines(raw$b6b,  col="blue")
plot(raw$b7a, type="l", col="red")
lines(raw$b7b,  col="blue")
plot(raw$b8a, type="l", col="red")
lines(raw$b8b,  col="blue")
plot(raw$b9a, type="l", col="red")
lines(raw$b9b,  col="blue")
plot(raw$ra, type="l", col="red")
lines(raw$rb,  col="blue")

################################################################################################
########################################Univariate effects######################################
################################################################################################

#unemp
write.table(data.frame(germany$theta,germany$unemp100), row.names=F, col.names=F,file="germanyUnemp_1_60_0.0_0.0_0.0.dat") 
#outofwed
write.table(data.frame(germany$theta,germany$outofwed2.100), row.names=F, col.names=F,file="germanyOutofwed_1_60_0.0_0.0_0.0.dat") 
#reunification
write.table(data.frame(germany$theta,germany$reunification), row.names=F, col.names=F,file="germanyReunification_1_60_0.0_0.0_0.0.dat") 
#partydummies
write.table(data.frame(germany$theta,germany$spd, germany$cducsu,germany$green,germany$pds), row.names=F, col.names=F,file="germanyParty_1_60_0.0_0.0_0.0_0.0_0.0_0.0.dat") 
#year
write.table(data.frame(germany$theta,c(germany$Year[2:60],germany$Year[1])), row.names=F, col.names=F,file="germanyYear_1_60_0.0_0.0_0.0.dat") 
#yearsq
write.table(data.frame(germany$theta,c(germany$Yearsq[2:60],germany$Yearsq[1])), row.names=F, col.names=F,file="germanyYearsq_1_60_0.0_0.0_0.0.dat") 

#We want to report the output in degrees, rather than radians. Function to do this:
convert.to.degrees <- function(output){
  print(mean(output))
  print(sd(output))
  print(quantile(output,c(0.025, 0.975)))
}

########Check convergence and calculate values for Table 6:
#unemp
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyUnemp_1_60_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyUnemp_1_60_0.0_0.0_0.0_WC.dat")
plot(rawWN[,4], type="l", col="red")
lines(rawWN[,5],col="blue")
plot(rawWC[,4], type="l", col="red")
lines(rawWC[,5],col="blue")
convert.to.degrees(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)))
convert.to.degrees(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)))
#outofwed
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyOutofwed_1_60_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyOutofwed_1_60_0.0_0.0_0.0_WC.dat")
plot(rawWN[,4], type="l", col="red")
lines(rawWN[,5],col="blue")
plot(rawWC[,4], type="l", col="red")
lines(rawWC[,5],col="blue")
convert.to.degrees(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)))
convert.to.degrees(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)))
#party
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyParty_1_60_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyParty_1_60_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat")
plot(rawWN[,4], type="l", col="red") #Change index to check convergence of other parties
lines(rawWN[,5],col="blue")
plot(rawWC[,4], type="l", col="red")
lines(rawWC[,5],col="blue")
convert.to.degrees(circular(c(rawWN[,2][3001:7000]*360/(2*pi),rawWN[,3][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,2][3001:7000]*360/(2*pi),rawWC[,3][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,6][3001:7000]*360/(2*pi),rawWN[,7][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,6][3001:7000]*360/(2*pi),rawWC[,7][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,8][3001:7000]*360/(2*pi),rawWN[,9][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,8][3001:7000]*360/(2*pi),rawWC[,9][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,10][3001:7000]*360/(2*pi),rawWN[,11][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,10][3001:7000]*360/(2*pi),rawWC[,11][3001:7000]*360/(2*pi)),unit="degrees"))

#reunification
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyReunification_1_60_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyReunification_1_60_0.0_0.0_0.0_WC.dat")
plot(rawWN[,4], type="l", col="red")
lines(rawWN[,5],col="blue")
plot(rawWC[,4], type="l", col="red")
lines(rawWC[,5],col="blue")
convert.to.degrees(circular(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)),unit="degrees"))

#year - we take the modulo
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyYear_1_60_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyYear_1_60_0.0_0.0_0.0_WC.dat")
plot((rawWN[,4]+pi)%%(2*pi), type="l", col="red")
lines((rawWN[,5]+pi)%%(2*pi),col="blue")
plot((rawWC[,4]+pi)%%(2*pi), type="l", col="red")
lines((rawWC[,5]+pi)%%(2*pi),col="blue")
convert.to.degrees(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)))
convert.to.degrees(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)))

#yearsq
rawWN <- read.table("Univariate models/raw_output_7000_1_germanyYearsq_1_60_0.0_0.0_0.0_WN.dat")
rawWC <- read.table("Univariate models/raw_output_7000_1_germanyYearsq_1_60_0.0_0.0_0.0_WC.dat")
plot(rawWN[,4], type="l", col="red")
lines(rawWN[,5],col="blue")
plot(rawWC[,4], type="l", col="red")
lines(rawWC[,5],col="blue")
convert.to.degrees(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)))
convert.to.degrees(c(rawWC[,4][3001:7000]*360/(2*pi),rawWC[,5][3001:7000]*360/(2*pi)))


#############################################################################################
###################################Final Restricted Model####################################
#############################################################################################

#The restricted model contains the party dummies and Year:
GERMANY2 <- cbind(germany$theta, germany$spd, 
                 germany$cducsu, germany$green, germany$pds,c(germany$Year[2:60],germany$Year[1]))


GERMANY2 <- as.data.frame(GERMANY2)
names(GERMANY2) <- c("direction", "spd", "cducsu", "green","pds", "year")

#Export data for full model
write.table(GERMANY2, row.names=F, col.names=F,file="germanyRestricted_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0.dat") 


################################Import sampling results###################################################
autocorWN <- read.table("Final restricted model/autocor_4000_1_germanyRestricted_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat", header=F)
names(autocorWN)<- c("lag", "b0", "b1","b2","b3","b4","b5","rho")
autocorWC <- read.table("Final restricted model/autocor_4000_1_germanyRestricted_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat", header=F)
names(autocorWC)<- c("lag", "b0", "b1","b2","b3","b4","b5","rho")

#change parameter name to check other parameters
plot(autocorWN$b5[1:500]~autocorWN$lag[1:500], col="red", type="l")
lines(autocorWN$b5[501:1000]~autocorWN$lag[501:1000], col="blue")
plot(autocorWN$rho[1:500]~autocorWN$lag[1:500], col="red", type="l")
lines(autocorWN$rho[501:1000]~autocorWN$lag[501:1000], col="blue")

plot(autocorWC$b5[1:500]~autocorWC$lag[1:500], col="red", type="l")
lines(autocorWC$b5[501:1000]~autocorWC$lag[501:1000], col="blue")
plot(autocorWC$rho[1:500]~autocorWC$lag[1:500], col="red", type="l")
lines(autocorWC$rho[501:1000]~autocorWC$lag[501:1000], col="blue")


##################Trace plots of data for all variables#############################
rawWN <- read.table("Final restricted model/raw_output_7000_1_germanyRestricted_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat", header=F)
names(rawWN) <- c("i","b0a","b0b","b1a","b1b","b2a", "b2b","b3a","b3b","b4a","b4b","b5a","b5b","ra","rb")
rawWC <- read.table("Final restricted model/raw_output_7000_1_germanyRestricted_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat", header=F)
names(rawWC) <- c("i","b0a","b0b","b1a","b1b","b2a", "b2b","b3a","b3b","b4a","b4b","b5a","b5b","ra","rb")

plot(rawWN$b1a, type="l", col="red")
lines(rawWN$b1b,  col="blue")
plot(rawWN$b2a, type="l", col="red")
lines(rawWN$b2b,  col="blue")
plot(rawWN$b3a, type="l", col="red")
lines(rawWN$b3b,  col="blue")
plot(rawWN$b4a, type="l", col="red")
lines(rawWN$b4b,  col="blue")
plot(rawWN$b5a, type="l", col="red", ylim=c(-5,5))
lines(rawWN$b5b,  col="blue")
plot(rawWN$ra, type="l", col="red")
lines(rawWN$rb,  col="blue")

plot(rawWC$b1a, type="l", col="red")
lines(rawWC$b1b,  col="blue")
plot(rawWC$b2a, type="l", col="red")
lines(rawWC$b2b,  col="blue")
plot(rawWC$b3a, type="l", col="red")
lines(rawWC$b3b,  col="blue")
plot(rawWC$b4a, type="l", col="red")
lines(rawWC$b4b,  col="blue")
plot(rawWC$b5a, type="l", col="red", ylim=c(-7,5))
lines(rawWC$b5b,  col="blue")
plot(rawWC$ra, type="l", col="red")
lines(rawWC$rb,  col="blue")

#Convert output to degrees to obtain results in Table 7
convert.to.degrees(circular(c(rawWN[,2][3001:7000]*360/(2*pi),rawWN[,3][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,4][3001:7000]*360/(2*pi),rawWN[,5][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,6][3001:7000]*360/(2*pi),rawWN[,7][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,8][3001:7000]*360/(2*pi),rawWN[,9][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(circular(c(rawWN[,10][3001:7000]*360/(2*pi),rawWN[,11][3001:7000]*360/(2*pi)),unit="degrees"))
convert.to.degrees(c(rawWN[,12][3001:7000]*360/(2*pi),rawWN[,13][3001:7000]*360/(2*pi)))


#Convergence problems for time in WC, therfore the final model based on WC should not include time:
write.table(data.frame(germany$theta,germany$spd, germany$cducsu, germany$green, germany$pds), row.names=F, col.names=F,file="germanyRestrictedWC_1_60_0.0_0.0_0.0_0.0_0.0_0.0.dat") 


##########################Non-Bayesian check of Residuals###############################

#For WN model:
y_predicted <- 2.8 + 1.52*GERMANY2$spd -2.95*GERMANY2$cducsu + 0.89*GERMANY2$green + 1.05*GERMANY2$pds + 0.002*GERMANY2$year

residuals <- (GERMANY2$direction-y_predicted)%%(2*pi)
plot(circular(residuals))

#WN transformation (see Mardia & Jupp p. 115)
WNtransformed <- 2*pi*sapply(residuals, function(y) integrate(function(x) dwrappednormal(x, mu=circular(0), rho=0.76), lower=0, upper=y)$value)
plot(circular(WNtransformed))


#For WC model:
y_predicted <- 2.99 + 1.39*GERMANY2$spd -2.93*GERMANY2$cducsu + 0.80*GERMANY2$green + 0.92*GERMANY2$pds 

residuals <- (GERMANY2$direction-y_predicted)%%(2*pi)
plot(circular(residuals))

#WC transformation:
WCtransformed <- 2*pi*sapply(residuals, function(y) integrate(function(x) dwrappedcauchy(x, mu=circular(0), rho=0.75), lower=0, upper=y)$value)
plot(circular(WCtransformed))

watson.test(circular(WNtransformed))
watson.test(circular(WCtransformed))



##########################Post predictive check of Residuals###############################
#function to calculate the watson U2 for the set of parameter values in a given iteration for chain 0 or 1
#calculate.watson.statistic <- function(parameter.values, chain, method){
#  parameter.values <- as.numeric(parameter.values)
#  y_predicted      <- parameter.values[chain+1] + parameter.values[chain+3]*GERMANY2$unemp + parameter.values[chain+5]*GERMANY2$spd +parameter.values[chain+7]*GERMANY2$cducsu + parameter.values[chain+9]*GERMANY2$green + parameter.values[chain+11]*GERMANY2$pds
#  residuals        <- (GERMANY2$direction-y_predicted)%%(2*pi)
  
#  if(method=="WN"){
#    residuals.transformed <- 2*pi*sapply(residuals, function(y) integrate(function(x) dwrappednormal(x, mu=circular(0), rho=parameter.values[chain+13]), lower=0, upper=y)$value)
    
#  }
#  if(method=="WC"){
#    residuals.transformed <- 2*pi*sapply(residuals, function(y) integrate(function(x) dwrappedcauchy(x, mu=circular(0), rho=parameter.values[chain+13]), lower=0, upper=y)$value)
#  }
  
#  return(watson.test(circular(residuals.transformed))$statistic)
#}

#WN
#read data:
#outWN <- read.table("Restricted model/output_foranalysis_4000_1_germany_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WN.dat")

#apply the calculate.watson.statistic to every row of the output:
#watson.statisticsWN_chain1 <- apply(outWN, 1, function(x) calculate.watson.statistic(x,0, "WN"))
#watson.statisticsWN_chain2 <- apply(outWN, 1, function(x) calculate.watson.statistic(x,1, "WN"))
#watson.statisticsWN <- c(watson.statisticsWN_chain1,watson.statisticsWN_chain2)
#hist(watson.statisticsWN)
#mean(watson.statisticsWN>watson.statistic_dataWN)

#WN
#read data:
#outWC <- read.table("Restricted model/output_foranalysis_4000_1_germany_1_60_0.0_0.0_0.0_0.0_0.0_0.0_0.0_WC.dat")

#apply the calculate.watson.statistic to every row of the output:
#watson.statisticsWC_chain1 <- apply(outWC, 1, function(x) calculate.watson.statistic(x,0, "WC"))
#watson.statisticsWC_chain2 <- apply(outWC, 1, function(x) calculate.watson.statistic(x,1, "WC"))
#watson.statisticsWC <- c(watson.statisticsWC_chain1,watson.statisticsWC_chain2)
#hist(watson.statisticsWC)
#mean(watson.statisticsWC>watson.statistic_dataWC)

#write.table(data.frame(WN=watson.statistic_dataWN,WC=watson.statistic_dataWC), file="watson.statistics.dat", row.names=F, col.names=T)


