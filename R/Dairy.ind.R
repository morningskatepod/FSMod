# Individual dairy
setwd("C:/Users/dougl/Documents/PhD Papers/004 - Farm Model")
start_time <- Sys.time()
# Read input file
source('dairy.ind.text')

iDay <- 1
nDay <- iDay


################ Predicting DMI (Fox et al. 2004) ###############

# CETI (Fox & Tylutki 1998)
CETI <- 27.88 - (0.456 * Temp) + (0.010754 * Temp^2)- (0.4905 * RH) + (0.00088 * RH^2)+ (1.1507 * WS) - (0.126447 * WS^2)+ (0.019876 * Temp * RH)- (0.046313 * Temp * WS)+ (0.4167 * HRS)
DMINC <- (119.62 - 0.9708 * CETI)/100
DMIAF_temp <- if(Temp > 20) {
  DMINC
} else {1.0433 - (0.0044 * Temp) + (0.0001 * Temp^2)}

calf_NEma <- (1.37 * calf_ME) - (0.138 * calf_ME^2) + (0.0105 * calf_ME^3) - 1.12
heifer_NEma <- (1.37 * heifer_ME) - (0.138 * heifer_ME^2) + (0.0105 * heifer_ME^3) - 1.12

#correction for breed index (fox et al 2004)
BI <- ifelse(BI == 1, 1.08,1)

#Calves
calf_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma^2)-0.1128)/calf_NEma)*DMIAF_temp*BI
  return(DMI)
}
#Yearlings
heifer_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*heifer_NEma)*(0.0466*heifer_NEma^2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
  return(DMI)
}
#Lactating Cows
cow_lact_DMI <- function(BW) {
  DMI <- ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
  return(DMI)
}
#Dry Cows
cow_dry_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (0.0185 * SBW)*DMIAF_temp*BI
  return(DMI)
}
####################### Excretion Calculations ############

# Nitrogen excreted (Nennich et al. 2005)
calf.N <- function(DMI) {
  return((DMI*calf_CP*112.55))
}

heifer.N <- function(DMI) {
  return((DMI*heifer_CP*78.39+51.4))
}

cow.lact.N<- function(DMI,BW) {
  return(((DMI*lact_CP*84.1)+(BW*0.196)))
}

#Use beef equation
cow.dry.N <- function(DMI) {
  return((DMI*dry_CP*78.39+51.4))
}

#Return adg as function of time for female cows 0 - 2500 days
dairy_ADG <- function(t) { #From Perotto et al, 1992
  A <- 619 #asymptotic weight, kg
  k <- 0.0020 #Rate parameter
  b <- 0.905 # integration constant
  M <- 1.2386 # inflection parameter
  # return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
  ifelse(t > 2500, return(0),
  return((A*(1-(b*exp(-k*t)))^M)/1000))
}

#For cumulative BW
area <- function(days) integrate(dairy_ADG, lower = 0, upper = ifelse(days<2500, days, 2500))$value
v.area <- Vectorize(area)


# Combine all equations to predict DMI
dairy_DMI <- function(df) {
  df$DMI = calf_DMI(df$BW)*(df$days < 365) + heifer_DMI(df$BW)*(df$days > 365 & df$days <= 730) +
    cow_lact_DMI(df$BW)*(df$days >=730 & df$days%%365 <= 305) +
    cow_dry_DMI(df$BW)*(df$days >= 730 & df$days%%365 > 305)
  return(df$DMI)
}

# Predict MY
dairy_MY <- function(df) {
  df$wol = (df$days > 730 & df$days %% 365 <= 305)*(df$days%%365/7)
  PKYDa = (0.125*df$RMP+0.375)*PKYD
  A = 1/(PKYDa*(1/8.5)*exp(1))
  Milk = df$wol/(A*exp((1/8.5)*df$wol))
  Milk = Milk*(df$days < 365*3)*0.74 + Milk*(df$days < 365*4 & df$days >= 365*3)*0.88+ Milk
  df$MY = Milk*(0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM
  return(df$MY)
}

# Combine all equations to predict N
dairy_N <- function(df) {
  df$N <- calf.N(df$DMI)*(df$days < 365) + heifer.N(df$DMI)*(df$days > 365 & df$days <= 730) +
    cow.lact.N(df$DMI, df$BW)*(df$days >=730 & df$days%%365 <= 305) +
    cow.dry.N(df$DMI)*(df$days >= 730 & df$days%%365 > 305)
  return(df$N)
}

# Predict pregnancy
# takes rate of preg success / # of days they will try
# if shes marked to cull, won't get preg
# If shes on day 0, give birth
dairy_preg <- function(df, preg = 0) {
  if('preg' %in% names(df) == F) {df$preg = preg}
  df$preg = (df$days%%365 != 0 & df$preg == 1)*1 +
    ifelse((runif(n = nrow(df), 0,1)<kPreg/220)*(df$days > 365 & df$days%%365 <= 280 & df$days%%365 > 60 & preg == 0),1,0)
return(df$preg)
}

#Things for Production Above Replacement (PAR)
# survival <- function()
FV <- function(df) {
  cor = 0.3
  sum = 0
  PKYDa = (0.125*5+0.375)*PKYD
  A = 1/(PKYDa*(1/8.5)*exp(1))
  currentlact = (sum((seq(pmin(df$days%%365,304),305,1)%%365/7)/(A*exp((1/8.5)*
                  (seq(pmin(df$days%%365,304),305,1)%%365/7))))*
                   (0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM)*((((df$days/365<3)*0.74))+((df$days/365<4)*0.88)+((df$days/365>=4)*1))
  first = (sum((seq(1,305,1)%%365/7)/(A*exp((1/8.5)*(seq(1,305,1)%%365/7))))*
    (0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM*0.74)*(df$days < 365*2)
  second = (sum((seq(1,305,1)%%365/7)/(A*exp((1/8.5)*(seq(1,305,1)%%365/7))))*
    (0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM*0.88*0.227*kPreg/100)*(df$days < 365*3)
  third = (sum((seq(1,305,1)%%365/7)/(A*exp((1/8.5)*(seq(1,305,1)%%365/7))))*
    (0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM*
    ifelse(df$days < 365*2,0.1148, 0.0261)*kPreg/100)*(df$days < 365*4)
  fourth = (sum((seq(1,305,1)%%365/7)/(A*exp((1/8.5)*(seq(1,305,1)%%365/7))))*
    (0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM*
    ifelse(df$days < 365*2,0.0018238, ifelse(df$days < 365*3,0.0080266,0.07))*kPreg/100)*(df$days < 365*5)

  return(sum)
}


FP <- function(df) {
  a = ((1-dweibull(x = pmax(0,((df$days/365)-1)), shape = 1.5, scale = 1.0))-
         (1-dweibull(x = pmax(0,((df$days/365)-2)), shape = 1.5, scale = 1.0)))/
    (1-(1-dweibull(x = pmax(0,((df$days/365)-2)), shape = 1.5, scale = 1.0)))
  a[is.nan(a)] <- 1
  a[is.infinite(a)] <- 0
  return(a)
}

new.group <- function(ids, ages, preg, prog = 1) {
#Make initial data frame - lactating
df.cows <- data.frame(CowID = ids,
                      days = sample(ages, length(ids), replace = T),
                      var = rnorm(n = length(ids),mean = 0, sd = 10),
                      RMP = round(runif(n = length(ids), min = 1, max = 9)),
                      tSCC = rnorm(n = length(ids), mean = 1, 0.4),
                      cECM = rnorm(n = length(ids), mean = ((prog-1)*cor)+1, 0.4),
                      Fat = rnorm(n = length(ids), mean = MilkFat, sd = 0.1),
                      Protein = rnorm(n = length(ids), mean = MilkProtein, sd = 0.1),
                      out = rep(0,length(ids)))
df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
df.cows$BW <- v.area(df.cows$days)*(((df.cows$var)/100)+1)
df.cows$DMI <- dairy_DMI(df.cows)
df.cows$MY <- dairy_MY(df.cows)
df.cows$N <- dairy_N(df.cows)
df.cows$preg <- dairy_preg(df.cows, preg = 0)
return(df.cows)
}

## Funciton to collect data
collect_data <- function(nDay) {
  outputs = list()
  outputs[1] <- sum(df.cows$MY)
  outputs[2] <- sum(df.cows$N)
  outputs[3] <- nrow(df.cows[df.cows$days < 365,])
  outputs[4] <- nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 305,])
  outputs[5] <- nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 > 305,])
  outputs[6] <- sum(df.cows$DMI)
  outputs[7] <- nrow(df.cows[df.cows$days >= 365 & df.cows$days < 730,])
  outputs[8] <- mean(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 60,"out"])
  outputs[9] <- mean(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 305,"preg"])
  outputs[10] <- target
  return(unlist(outputs))
}


### Start
set.seed(1)
total.start.cows = 1000

lactating_ages <- c((365*2):(365*2+305),(365*3):(365*3+305),
                    (365*4):(365*4+305),(365*5):(365*5+305))
other_ages <- 1:(365*5+305)
other_ages <- other_ages[-lactating_ages]
# correlation of cECM to young
cor = 0.3

#Make initial data frame - lactating
df.cows <- new.group(ids = seq(1,total.start.cows,1),
                     ages = sample(lactating_ages, total.start.cows, replace = T),
                     preg = 0)

# non lactating
df.cows2 <- new.group(ids = seq(total.start.cows+1,total.start.cows+round(total.start.cows*0.17),1),
                     ages = sample(1:365, round(total.start.cows*0.17), replace = T),
                     preg = 0)

df.cows <- rbind(df.cows, df.cows2)

df.cows <- rbind(df.cows, new.group(ids = seq(nrow(df.cows)+1,nrow(df.cows)+round(total.start.cows*0.17),1),
                                  ages = other_ages, preg = 1))
# Calculate mean MY for herd
mean_MY = mean(df.cows[df.cows$days > 730,]$MY)*365

# # Plot Stats
par(mfrow= c(2,2))
par(mar = c(3,4,3,4))
# plot(df.cows$days, df.cows$ADG, ylab = 'ADG')
# plot(df.cows$days, df.cows$BW, ylab = 'BW')
# plot(df.cows$days, df.cows$DMI, ylab = 'DMI')
# plot(df.cows$days, df.cows$N, ylab = 'N, per day')

### Running Simulation
n = rep(0, fDay)

# DFs to collect info suring run
dead <- data.frame()
# outputs <- data.frame(Milk = rep(0,fDay), N = rep(0,fDay),
#                       calves = rep(0,fDay), lact = rep(0,fDay), dry = rep(0,fDay),
#                       age_lact = rep(0, fDay),heifer = rep(0, fDay))
outputs <- matrix(nrow = fDay, ncol = 10)
off = 0
target = 0

# ETA
paste('projected completion time:',(total.start.cows*fDay*60/1000*0.01)+start_time)

# Loop
while(nDay <= fDay) {
  # Collect data
  outputs[nDay,] <- collect_data(nDay)

  # Marking/Culling
  currentMark <- mean(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 60,"out"])
  heifers <- nrow(df.cows[df.cows$days > (730- markFreq) & df.cows$days < 730,])
  dry <- nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 > 365 - markFreq,])
  lastOff = off
  off = (nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 305,]) -
           total.start.cows)
  target = round(sqrt(max(0,off + heifers + dry)))

  #Mark every x days
  if(nDay %% markFreq == 0) {

    over = max(0,target)
    possible_cull = df.cows[(df.cows$preg == 0 & df.cows$days >=365*2 & df.cows$days%%365 <= 60),]

    # Culling strats
    if(cullStrat == 1) {
      idsCull <- possible_cull[order(possible_cull$cECM),][1:over,'CowID']
      df.cows[df.cows$CowID %in% idsCull, 'out'] <- 1
    } else if(cullStrat == 2) {
      idsCull <- possible_cull[order(possible_cull$days, decreasing = T),][1:over,'CowID']
      df.cows[df.cows$CowID %in% idsCull, 'out'] <- 1
    } else {
      possible_cull[order(possible_cull$cECM),][1:over,'out'] <- 1 # Needs changed
    }
  }

  # Decide which cows will be dead before next lactation

  # Check to see if any cows calved
  new_calves <- df.cows[df.cows$preg == 1 & df.cows$days %% 365 == 0 & runif(1,0,1) < (rateFemale/100),]
  df.cows[(df.cows$preg == 0 & df.cows$days > 730 & df.cows$days %% 365 == 0 & runif(nrow(df.cows),0,1) < FP(df.cows)),'out'] <- 1
  # If new female calves
  # make new df for those calves related to their mothers traits
  if(nrow(new_calves)>0) {
    df.calves.new <- new.group(ids = new_calves$CowID+(total.start.cows*10),
                              ages = rep(1, nrow(new_calves)), preg = 0,
                              prog = new_calves$cECM)

    # add calves to full group
    df.cows <- rbind(df.cows, df.calves.new)
  }

  # if cows dry w/o getting pregnant or are marked, cull now
  new_cows <- df.cows[df.cows$out == 1 & df.cows$days %% 365 == 305,]
  #Keep track of who leaves herd
  dead <- rbind(dead, new_cows)


  # remove culled cows from list
  df.cows <- if(nrow(new_cows) > 0) {df.cows[!(df.cows$CowID %in% new_cows$CowID),]} else {df.cows}

  # if any cows culled and total herd is below strating #, add more heifers
  if(off < 0 & nrow(new_cows) > 0) {
    df.cows.new <- new.group(new_cows$CowID+total.start.cows,
                             rep(365*2, nrow(new_cows)),
                             preg = 1)
    df.cows <- rbind(df.cows, df.cows.new)
  }



  #Update other fields
  df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
  df.cows$BW <- v.area(df.cows$days)
  df.cows$DMI <- dairy_DMI(df.cows)
  df.cows$MY <- dairy_MY(df.cows)
  df.cows$N <- dairy_N(df.cows)
  df.cows$preg <- dairy_preg(df.cows)

  #change day # for cows and counter
  df.cows$days <- df.cows$days + 1
  nDay = nDay+1
  if(outputs[nDay-1,4] > total.start.cows*2) {break}
}


end_time <- Sys.time()

## Plotting results

moving_avg <- function(x, n) {
  cx <- c(rep(NA,365),cumsum(x))
  return((cx[(365+1):length(cx)] - cx[1:(length(cx) - 365)]) / 365)
}

rsum <- moving_avg(x = outputs[,8], n = 365)
plot(outputs[,8], type = 'l', ylab = '% Marked in Lact', xlim = c(0,fDay))
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs[,1]/total.start.cows, n = 365)
plot(outputs[,1]/total.start.cows, type = 'l',
     xlim = c(0,fDay),ylab = 'Milk/Lactating Herd Size')
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs[,2]/total.start.cows, n = 365)
plot(outputs[,2]/total.start.cows, type = 'l',
     xlim = c(0,fDay), ylab = 'N/Lactating Herd Size')
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs[,4], n = 365)
plot(outputs[,4], type = 'l', ylim = c(0,total.start.cows*1.2),
     xlim = c(0,fDay), ylab = '# of Cows')
lines(rsum, col = 'orange', lwd = 2, lty = 2)
lines(outputs[,5], col = 'red') #dry
lines(outputs[,3], col = 'blue') #calves
lines(outputs[,7], col = 'green') #heifers
abline(h = total.start.cows)

paste(round((end_time - start_time)/total.start.cows/fDay*1000,5), "minutes per 1000 cow days,",
      round((end_time - start_time),2),'Mins for', (fDay*total.start.cows), 'Total cow days')

df.cows$YOL <- df.cows$days/365-1
paste('average lactation:',round(mean(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 305,'YOL']),2))
