setwd("C:/Users/dougl/Documents/PhD Papers/Farm Model")
############################################
# Set all rates to 0 before reading files
kMature <- 0
kBirth <- 0
kCull <- 0
kDry <- 0
kFreshening <- 0
kMilk <- 0
kMature <- 0
kBirth <- 0
kCull <- 0
kLaying <- 0
kMortality <- 0
# Set outputs to 0
Meat <- 0
Milk <- 0
Eggs <- 0
CumSumWt <- 0
N <- 0

# Set animal numbers to 0
nCalf <- 0
nHeifer <- 0
nLact <- 0
nDry <- 0
nBroilers <- 0
nChicks <- 0
wtCalf <- 0
wtHeifer <- 0
wtLact <- 0
wtDry <- 0
wtBroilers <- 0
wtChicks <- 0

# Read input file
source('broiler.input.text')
iDay <- 1
nDay <- iDay

#############  BROILERS  ##################
if(animal == 'broiler') {

nChicks <- iChicks #Set starting count of young
wtChicks <- 0.025*nChicks #each chick = 25g
nBroilers <- 0
wtBroilers <- 0

# Flux rates
kMature <- 1 #when chicks mature, they all do at once
kBirth <- 1 #When chicks hatch, get all new chicks
kCull <- 1 #when we sell, all sell
kMortality <- kMortality*iChicks/switch_feed*2/100

broiler_ADG <- function(Temp) {
# # Use this for ADG calculations
# # For use as BW in ADG eq.
wt_per_bird <- sum(wtChicks,wtBroilers)/sum(nChicks,nBroilers)
wt_per_bird <- wt_per_bird*1000

#May et al 1998
ADG <- -31.797 + (1.2071*Temp) + (0.21457*wt_per_bird) - (8.852E-5*wt_per_bird^2) +
  (1.51E-8*wt_per_bird^3) - (2.0772E-3*Temp*wt_per_bird)

return(ADG/1000)
}

#Zuidhof et al 2014
broiler_Intake <- function(ADG) {
  I_ME <- (196*(sum(wtChicks,wtBroilers)/sum(nChicks,nBroilers))^0.7543)+2.4833*ADG*1000 #Convert to g
  ME <- if(nChicks > 0) {ME_young} else {ME_mature}
  Intake <- I_ME/ME
  return(Intake)
}

broiler_N_excretion <- function(ADG) {
  CP <- ifelse(nChicks>0, CPy,CPo)
  FI <- broiler_Intake(ADG)
  # For Mature Birds
  if(nBroilers > 0) {
  Nintake <- FI*1000 * nBroilers * (CP/100)/6.25
  Nret <- 29 * (ADG*nBroilers) # Constant 29 g/kg of BW gain according to ITAVI, 2013
  Nexc <- Nintake - Nret # Formula from Belloir et al. 2017
  return(Nexc/1000) # Returns N in kg
  }
  # For young birds
  else {
    Nintake <- FI * nChicks * (CP/100)/6.25 #N in kg
    Nexc <- 0.589*Nintake*1000 - 5.004 #Bregendahl et al. 2002 using N, grams
    return(Nexc/1000)  # Returns N in kg
  }
}
}

##################  DAIRY  #####################
if(animal == 'dairy') {

kMature <- 1/574
kBirth <- kCull # add cows into herd at same rate as those leave
kDry <- 1/305
kFreshening <- 1/305


kMilk <- 45

#Start counts
nCalf <- iCalf #Set starting count of young
nHeifer <- iHeifer
nLact <- iLact
nDry <- iDry

#Return adg as function of time for female cows 0 - 2500 days
calf_ADG <- function(t) { #From Perotto et al, 1992
  A <- 619 #asymptotic weight, kg
  k <- 0.0020 #Rate parameter
  b <- 0.905 # integration constant
  M <- 1.2386 # inflection parameter
  return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
}

#### Predicting DMI for four groups of dairy cows on farm (Fox et al. 2004) ###

# CETI (Fox & Tylutki 1998)
CETI <- 27.88 - (0.456 * Temp) + (0.010754 * Temp^2)- (0.4905 * RH) + (0.00088 * RH^2)+ (1.1507 * WS) - (0.126447 * WS^2)+ (0.019876 * Temp * RH)- (0.046313 * Temp * WS)+ (0.4167 * HRS)
DMINC <- (119.62 - 0.9708 * CETI)/100
DMIAF_temp <- if(Temp > 20) {
  DMINC
} else {1.0433 - (0.0044 * Temp) + (0.0001 * Temp^2)}

calf_NEma <- (1.37 * calf_ME) - (0.138 * calf_ME^2) + (0.0105 * calf_ME^3) - 1.12
yearling_NEma <- (1.37 * yearling_ME) - (0.138 * yearling_ME^2) + (0.0105 * yearling_ME^3) - 1.12

#correction for breed index (fox et al 2004)
BI <- ifelse(BI == 1, 1.08,1)

#Calves
calf_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*calf_NEma)*(0.0466*calf_NEma^2)-0.1128)/calf_NEma)*DMIAF_temp*BI
}
#Yearlings
yearling_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*yearling_NEma)*(0.0466*yearling_NEma^2)-0.0869)/yearling_NEma)*DMIAF_temp*BI
}
#Lactating Cows
lact_cow_DMI <- function(BW) {
  DMI <- ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
}
#Dry Cows
dry_cow_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (0.0185 * SBW)*DMIAF_temp*BI
}

##Excretion Calculations

# Nitrogen excreted (Nennich et al. 2005)
calf_N_excretion <- function() {
  calf_DMI <- calf_DMI(wtCalf/nCalf)
  return((calf_DMI*calf_CP*112.55)*nCalf)
}
yearling_N_excretion <- function() {
  yearling_DMI <- yearling_DMI(wtHeifer/nHeifer)
  return((yearling_DMI*yearling_CP*78.39+51.4)*nHeifer)
}
lact_N_excretion <- function() {
  lact_DMI <- lact_cow_DMI(wtLact/nLact)
  return(((lact_DMI*lact_CP*84.1)+(wtLact/nLact*0.196))*nLact)
}
#Use beef equation
dry_N_excretion <- function() {
  dry_DMI <- dry_cow_DMI(wtDry/nDry)
  return((dry_DMI*dry_CP*78.39+51.4)*nDry)
}

}

#####################  BEEF #######################
if(animal == 'beef') {
kBirth <- 1/365
kMature <- 1/365
kCull <- 1/365

# Start Counts
nCalf <- iCalf
nBeef <- iBeef
nBeefCows <- iBeefCows

beef_ADG <- function() {

}

beef_DMI <- function(Temp) {

}

beef_N_excretion <- function() {

}

}

################### SWINE ##########################
if(animal == 'swine') {
kBirth <- 1/365
kMature <- 1/365
kCull <- 1/365

# Start Counts
nPiglet <- iPiglet
nSow <- iSow
nBoar <- iBoar

swine_ADG <- function() {

}

swine_DMI <- function(Temp) {

}

swine_N_excretion <- function() {

}

}

################### LAYERS ##########################
if(animal == 'layer') {
kBirth <- 1/365
kMature <- 1/365
kCull <- 1/365

# Start Counts
nChicks <- iChicks
nLayers <- iLayers

layer_ADG <- function() {

}


layer_N_excretion <- function() {

}

}
#####################  FOR LOOP #######################

#Write an array
val <- array(1, dim = c(10, length(seq(iDay,fDay, by = 1))))

while(nDay <= fDay) {
  if(animal == 'broiler') {

    # Set t for broilers, know switch times
    t <- ifelse(nDay%%(switch_feed*2) == 0,2, ifelse(nDay%%switch_feed ==0,1,0))

    # Rates
    born <- (t==2)*kBirth*iChicks
    maturing <- (t==1)*kMature*nChicks #t switches for broilers
    culling <- (t==2)*kCull*nBroilers
    # laying <- kLaying*nLayers

    ADG <- broiler_ADG(Temp)
    Nexc <- broiler_N_excretion(ADG)


    # Update Numbers of Animals
    nBroilers <- nBroilers+(maturing-culling)-(kMortality*(nBroilers > 0))
    nChicks <- nChicks+(born-maturing)-(kMortality*(nChicks > 0))
    meat_produced <- culling*(wtBroilers)

    wtBroilers <- (nBroilers > 0)*(wtBroilers + ADG*nBroilers + (t==1)*wtChicks)
    wtChicks <- (nChicks > 0)*(wtChicks + ADG*nChicks)
    if(t==2) {wtChicks <- 0.025*nChicks} #reset new chicks
  }

  if(animal == 'dairy') {
    born <- kBirth*nLact*0.5
    maturing <- kMature*nCalf
    freshening <- kFreshening*nDry+kFreshening*nHeifer
    drying <- kDry*nLact
    culling <- kCull*nLact
    Nexc <- calf_N_excretion()+yearling_N_excretion()+lact_N_excretion()+dry_N_excretion()

    #Update Numbers of Animals
    nCalf <- nCalf #+ born
    nHeifer <- nHeifer #+ maturing
    nLact <- nLact #+ freshening
    nDry <- nDry #+ drying
    meat_produced <- culling*(wtLact)
  }

  # New Totals
  Meat <- Meat + meat_produced
  # Eggs <- Eggs + laying*nLayers
  # Milk <- Milk + kMilk*nLact
  N <- N + Nexc

  # Update Numbers of Animals



  # Update values
  val[1,nDay] <- nCalf
  val[2,nDay] <- nHeifer
  val[3,nDay] <- nLact
  val[4,nDay] <- nDry
  val[5,nDay] <- nChicks
  val[6,nDay] <- nBroilers+nChicks
  val[7,nDay] <- wtBroilers+wtChicks
  val[8,nDay] <- ADG*1000
  val[9,nDay] <- N
  val[10,nDay] <- Meat

  nDay = nDay+1
}
##############################################


# Plot results
par(mfrow = c(1,3))
# plot(val[1,], main = 'Calves', xlab = 'Day', ylab = 'Count')
# plot(val[2,], main = 'Heifers', xlab = 'Day', ylab = 'Count')
# plot(val[3,], main = 'Lactating Cows', xlab = 'Day', ylab = 'Count')
# plot(val[4,], main = 'Dry Cows', xlab = 'Day', ylab = 'Count')
# plot(val[5,], main = 'Chicks', xlab = 'Day', ylab = 'Count')
# plot(val[6,], main = 'Broilers', xlab = 'Day', ylab = 'Count')
plot(val[7,], main = 'Broiler Weight', xlab = 'Day', ylab = 'Wt, kg')
plot(val[8,], main = "ADG", xlab = 'Day', ylab = 'g/d')
plot(val[9,], main = "N Excretion", xlab = 'Day', ylab = 'N, kg')
# plot(val[10,], main = "Meat", xlab = 'Day', ylab = 'Wt, kg')
