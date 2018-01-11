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
kMortality <- 0 #as whole number

# Set outputs to 0
Meat <- 0
Milk <- 0

# Set animal numbers to 0
nCalf <- 0
nHeifer <- 0
nLact <- 0
nDry <- 0
wtCalf <- 0
wtHeifer <- 0
wtLact <- 0
wtDry <- 0

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
