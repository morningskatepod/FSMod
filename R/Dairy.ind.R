# Individual dairy
setwd("C:/Users/dougl/Documents/PhD Papers/004 - Farm Model")

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
lact_cow_DMI <- function(BW) {
  DMI <- ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
  return(DMI)
}
#Dry Cows
dry_cow_DMI <- function(BW) {
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
area <- function(days) integrate(dairy_ADG, lower = 0, upper = ifelse(days<2500, days, 2500))$value+rnorm(n = 1, mean = 0, sd = min(days/20, 100))
v.area <- Vectorize(area)


# Combine all equations to predict DMI
dairy_DMI <- function(BW,days) {
  if(days <= 365) {return(calf_DMI(BW))}
  else if(days <= 730) {return(heifer_DMI(BW))}
  else if(days %% 365 <= 305) {return(lact_cow_DMI(BW))}
  else {return(dry_cow_DMI(BW))}
}

# Predict ECM from yield
ECM <- function(MY, Fat, Protein) {
  return(MY*(0.122*Fat+0.077*Protein+0.249))
}

# Predict MY
dairy_MY <- function(days, RMP, Fat, Protein, cECM) {
  week_of_lactation <- if(days > 730) {if(days %% 365 <= 305) {days%%365/7} else {return(0)}} else {return(0)}
  PKYDa = (0.125*RMP+0.375)*PKYD
  A = 1/(PKYDa*(1/8.5)*exp(1))
  Milk = week_of_lactation/(A*exp((1/8.5)*week_of_lactation))
  Milk <- if(days < 365*3) {Milk*0.74} else if(days < 365*4) {Milk*0.88} else {Milk}
  Milk <- ECM(Milk, Fat, Protein)*cECM # Correct for productivity
  return(Milk)
}

# Combine all equations to predict N
dairy_N <- function(DMI,BW, days) {
  if(days <= 365) {return(calf.N(DMI))}
  else if(days <= 730) {return(heifer.N(DMI))}
  else if(days %% 365 <= 305) {return(cow.lact.N(DMI,BW))}
  else {return(cow.dry.N(DMI))}
}

# Predict pregnancy
# takes rate of preg success / # of days they will try
# if shes marked to cull, won't get preg
# If shes on day 0, give birth
dairy_preg <- function(days,preg, out) {
  if(days%%365 == 0 & days > 365 & preg == 1) {return(0)}
  else if(out == 1) {return(0)}
  else if(preg == 1) {return(1)}
  else if(days%%365 <= 305) {return(ifelse(runif(n = 1, 0,100)<kPreg, 1, 0))}
  else {return(0)}
}

#Things for Production Above Replacement (PAR)
survival <- function()
FP <- function(days, RMP, Fat, Protein, silent = T) {
  cor = 0.3
  sum = 0
  if(days < 365*3) {
    next_lact = (round(days/365)+1)*365
    second = integrate(f = dairy_MY, lower = next_lact, upper = next_lact+305,
             RMP = RMP, Fat = Fat, Protein = Protein)$value
    second = (((second/mean_MY-1)*cor)+1)*mean_MY
    sum = sum + second
    for(i in 3:8) {
      third = integrate(f = dairy_MY, lower = next_lact+365, upper = next_lact+365+305,
                       RMP = RMP, Fat = Fat, Protein = Protein)$value
      third = (((third/mean_MY-1)*(cor^(i-1)))+1)*mean_MY
      sum = sum + third
    }
  } else {for(i in (round(days/365)+1):8) {
    third = integrate(f = dairy_MY, lower = next_lact+365, upper = next_lact+365+305,
                     RMP = RMP, Fat = Fat, Protein = Protein)$value
    third = (((third/mean_MY-1)*(cor^(i-1)))+1)*mean_MY
    sum = sum + third
  }}
  return(sum)
}


### Start
set.seed(1)
total.start.cows = 100

#Make initial data frame
df.cows <- data.frame(CowID = seq(1,total.start.cows,1),
                      days = sample(1:4000, total.start.cows, replace = T),
                      RMP = round(runif(n = total.start.cows, min = 1, max = 9)),
                      tSCC = rnorm(n = total.start.cows, mean = 1, 0.4),
                      cECM = rnorm(n = total.start.cows, mean = 1, 0.4),
                      Fat = rnorm(n = total.start.cows, mean = MilkFat, sd = 0.1),
                      Protein = rnorm(n = total.start.cows, mean = MilkProtein, sd = 0.1),
                      out = rep(0,total.start.cows))
df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
df.cows$BW <- v.area(df.cows$days)
df.cows$DMI <- mapply(dairy_DMI, df.cows$BW, df.cows$days)
df.cows$MY <- mapply(dairy_MY, df.cows$days, df.cows$RMP, df.cows$Fat, df.cows$Protein, df.cows$cECM)
df.cows$N <- mapply(dairy_N, df.cows$DMI, df.cows$BW, df.cows$days)
df.cows$preg <- ifelse(df.cows$days > 365 & df.cows$days%%365 <= 305, 1, 0)

# Calculate mean MY for herd
mean_MY = mean(df.cows[df.cows$days > 730,]$MY)*365

# correlation of cECM to young
cor = 0.3

# Plot Stats
par(mfrow= c(2,2))
par(mar = c(3,4,3,4))
plot(df.cows$days, df.cows$ADG)
plot(df.cows$days, df.cows$BW)
plot(df.cows$days, df.cows$DMI)
plot(df.cows$days, df.cows$N)

### Running Simulation
n = rep(0, fDay)

# DFs to collect info suring run
dead <- data.frame()
outputs <- data.frame(Milk = rep(0,fDay), N = rep(0,fDay),
                      calves = rep(0,fDay), lact = rep(0,fDay), dry = rep(0,fDay),
                      age_lact = rep(0, fDay))
# Loop
while(nDay <= fDay) {
  # Collect data
  outputs[nDay,1] <- sum(df.cows$MY)
  outputs[nDay,2] <- sum(df.cows$N)
  outputs[nDay,3] <- nrow(df.cows[df.cows$days < 365,])
  outputs[nDay,4] <- nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 305,])
  outputs[nDay,5] <- nrow(df.cows[df.cows$days >= 730 & df.cows$days%%365 > 305,])
  outputs[nDay,6] <- mean(df.cows[df.cows$days >= 730 & df.cows$days%%365 <= 60,"days"])

  # Marking/Culling
  #Mark every x days
  if(nDay %% markFreq == 0) {
    over = (nrow(df.cows)-100)*2
    if(cullStrat == 1) {
      df.cows[order(df.cows$cECM),][1:over,'out'] <- 1
    } else if(cullStrat == 2) {
      df.cows[order(df.cows$days),][1:over,'out'] <- 1
    } else {
      df.cows[order(df.cows$cECM),][1:over,'out'] <- 1 # Needs changed
    }
  }

  # Check to see if any cows calved
  new_calves <- df.cows[df.cows$preg == 1 & df.cows$days %% 365 == 0 & runif(1,0,1) < (rateFemale/100),]

  # If new female calves
  # make new df for those calves related to their mothers traits
  if(nrow(new_calves) > 0) {
    df.calves.new <- data.frame(CowID = new_calves$CowID+(total.start.cows*10),
                              days = rep(1, nrow(new_calves)),
                              RMP = round(runif(n =  nrow(new_calves), min = 1, max = 9)),
                              tSCC = rnorm(n =  nrow(new_calves), mean = 1, 0.4),
                              cECM = rnorm(n = nrow(new_calves), mean =((new_calves$cECM-1)*cor)+1,sd = 0.4),
                              Fat = rnorm(n =  nrow(new_calves), mean = MilkFat, sd = 0.1),
                              Protein = rnorm(n =  nrow(new_calves), mean = MilkProtein, sd = 0.1),
                              out = rep(0, nrow(new_calves)))
    df.calves.new$ADG <- mapply(dairy_ADG, df.calves.new$days)
    df.calves.new$BW <- v.area(df.calves.new$days)
    df.calves.new$DMI <- mapply(dairy_DMI, df.calves.new$BW, df.calves.new$days)
    df.calves.new$MY <- mapply(dairy_MY, df.calves.new$days, df.calves.new$RMP, df.calves.new$Fat, df.calves.new$Protein, df.calves.new$cECM)
    df.calves.new$N <- mapply(dairy_N, df.calves.new$DMI, df.calves.new$BW, df.calves.new$days)
    df.calves.new$preg <- rep(0, nrow(new_calves))
    # add calves to full group
    df.cows <- rbind(df.cows, df.calves.new)
  }

  # if cows dry w/o getting pregnant or are marked, cull now
  new_cows <- df.cows[(df.cows$out == 1 | df.cows$preg == 0) & df.cows$days %% 365 == 305,]
  #Keep track of who leaves herd
  dead <- rbind(dead, new_cows)
  # collect total cows
  n[nDay] = nrow(new_cows)

  # remove culled cows from list
  df.cows <- if(nrow(new_cows) > 1) {df.cows[-as.numeric(rownames(new_cows)),]} else {df.cows}

  # if any cows culled and total herd is below strating #, add more heifers
  if(nrow(new_cows) > 0 & nrow(df.cows) < 100) {
    df.cows.new <- data.frame(CowID = new_cows$CowID+total.start.cows,
                              days = rep(365*2, nrow(new_cows)),
                              RMP = round(runif(n =  nrow(new_cows), min = 1, max = 9)),
                              tSCC = rnorm(n =  nrow(new_cows), mean = 1, 0.4),
                              cECM = rnorm(n =  nrow(new_cows), mean = 1, 0.4),
                              Fat = rnorm(n =  nrow(new_cows), mean = MilkFat, sd = 0.1),
                              Protein = rnorm(n =  nrow(new_cows), mean = MilkProtein, sd = 0.1),
                              out = rep(0, nrow(new_cows)))
    df.cows.new$ADG <- mapply(dairy_ADG, df.cows.new$days)
    df.cows.new$BW <- v.area(df.cows.new$days)
    df.cows.new$DMI <- mapply(dairy_DMI, df.cows.new$BW, df.cows.new$days)
    df.cows.new$MY <- mapply(dairy_MY, df.cows.new$days, df.cows.new$RMP, df.cows.new$Fat, df.cows.new$Protein, df.cows.new$cECM)
    df.cows.new$N <- mapply(dairy_N, df.cows.new$DMI, df.cows.new$BW, df.cows.new$days)
    df.cows.new$preg <- rep(0, nrow(new_cows))
    df.cows <- rbind(df.cows, df.cows.new)
  }



  #Update other fields
  df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
  df.cows$BW <- v.area(df.cows$days)
  df.cows$DMI <- mapply(dairy_DMI, df.cows$BW, df.cows$days)
  df.cows$MY <- mapply(dairy_MY, df.cows$days, df.cows$RMP, df.cows$Fat, df.cows$Protein, df.cows$cECM)
  df.cows$N <- mapply(dairy_N, df.cows$DMI, df.cows$BW, df.cows$days)
  df.cows$preg <- mapply(dairy_preg, df.cows$days, df.cows$preg, df.cows$out)


  #change day # for cows and counter
  df.cows$days <- df.cows$days + 1
  nDay = nDay+1
}

moving_avg <- function(x, n) {
  cx <- c(rep(NA,365),cumsum(x))
  return((cx[(365+1):length(cx)] - cx[1:(length(cx) - 365)]) / 365)
}


rsum <- moving_avg(x = outputs$Milk, n = 365)
plot(outputs$Milk, type = 'l')
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs$Milk/outputs$lact, n = 365)
plot(outputs$Milk/outputs$lact, type = 'l')
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs$N, n = 365)
plot(outputs$N)
lines(rsum, col = 'red', lwd = 2, lty = 2)

plot(outputs$lact, type = 'l', ylim = c(0,120), ylab = '# of Cows')
lines(outputs$dry, col = 'red')
lines(outputs$calves, col = 'blue')
