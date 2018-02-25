# setwd("C:/Users/dougl/Documents/PhD Papers/004 - Farm Model")
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
kLaying <- 0
kMortality <- 0 #as whole number

# Set outputs to 0
Meat <- 0
Milk <- 0
Eggs <- 0
CumSumWt <- 0
N <- 0
P <- 0

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


kMortality <- 7 #as whole number %
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
Temp <- if(Temp<=23) {23} else {if(Temp>31) {31} else{Temp}}

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

broiler_P_excretion <- function(ADG) {
  nP <- ifelse(nChicks > 0, nP_young, nP_mature)
  P <- ifelse(nChicks > 0, P_young, P_mature)
  FI <- broiler_Intake(ADG)
  P_exc <- exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  #Kornegay et al. 1996
               ((0.4088*log(nP))^2)+((-0.0087*log(P))^2)+
                 (0.0012*log(P)*log(nP))) #g/kg intake
  Pexc <- FI*P_exc*sum(nBroilers, nChicks)
  return(Pexc/1000) # Return P in kg
}
}


