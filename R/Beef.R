# setwd("C:/Users/dougl/Documents/PhD Papers/004 - Farm Model")
############################################

# Set all rates to 0 before reading files
kCow.birth <- 0
kCow.bred <- 0
kCow.dry <- 0
kCow.slaughter <- 0

kProd.steer.grow <- 0
kProd.cow.grow <- 0
kProd.steer.finish <- 0
kProd.cow.finish <- 0
kProd.slaughter <-  0 #same for bull & cow

kBreed.cow.grow <- 0
kBreed.bull.grow <- 0
kBreed.cow.mature <- 0
kBreed.bull.mature <- 0
kBreed.bull.slaughter <- 0


# Set outputs to 0

Meat <- 0

# Set animal numbers to 0

nCow.bred <- 0
nCow.dry <- 0
nCow.lact <- 0

nProd.bull.calf <- 0
nProd.cow.calf <- 0
nProd.steer.grow <- 0
nProd.cow.grow <- 0
nProd.steer.finish <- 0
nProd.cow.finish <- 0

nBreed.cow.calf <- 0
nBreed.bull.calf <- 0
nBreed.cow.grow <- 0
nBreed.bull.grow <- 0
nBreed.bull.mature <- 0

# Set animal weights


# Mortality Rates at any step
kMortality <- 7 #as a whole number


iDay <- 1
nDay <- iDay

# Read input file
source('beef.input.text')


##################  BEEF  #####################
if(animal == 'beef') {



  #Start counts
  nCow.bred <- iCow.bred
  nCow.dry <- iCow.dry
  nCow.lact <- iCow.lact

  nProd.bull.calf <- iProd.bull.calf
  nProd.cow.calf <- iProd.cow.calf
  nProd.steer.grow <- iProd.steer.grow
  nProd.cow.grow <- iProd.cow.grow


  nBreed.cow.calf <- iBreed.cow.calf
  nBreed.bull.calf <- iBreed.bull.calf
  nBreed.cow.grow <- iBreed.cow.grow
  nBreed.bull.grow <- iBreed.bull.grow
  nBreed.bull.mature <- iBreed.bull.mature

  # ADG Functions
  steer_ADG <- function(t) { #From DeNise & Brinks 1985
    A <- steer.max.wt #asymptotic weight, kg
    k <- 0.00219 #Rate parameter
    b <- 0.889 # integration constant
    M <- 1.25 # inflection parameter
    return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
  }
  cow_ADG <- function(t) {
    A <- cow.max.wt #asymptotic weight, kg
    k <- 0.00219 #Rate parameter
    b <- 0.889 # integration constant
    M <- 1.25 # inflection parameter
    return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
  }

  #### Predicting DMI for four groups of beef cattle on farm (Beef NRC 2000) ###

  # CETI (Fox & Tylutki 1998)
  CETI <- 27.88 - (0.456 * Temp) + (0.010754 * Temp^2)- (0.4905 * RH) + (0.00088 * RH^2)+ (1.1507 * WS) - (0.126447 * WS^2)+ (0.019876 * Temp * RH)- (0.046313 * Temp * WS)
  DMINC <- (119.62 - 0.9708 * CETI)/100
  DMIAF_temp <- if(Temp > 20) {
    DMINC
  } else {1.0433 - (0.0044 * Temp) + (0.0001 * Temp^2)}

  #correction for breed index (fox et al 2004)
  BI <- ifelse(Angus == 1, 1,ifelse(Holstein == 1, 1.08, 1.04))

  #Calves
  calf_DMI <- function(BW) {
    SBW <- 0.94*BW
    calf_NEma <- (SBW^0.75)*((0.077*(0.8+(CS - 1)*0.05))+(0.0007*(20-Temp)))
    DMI <- (SBW^0.75)*(((0.2435*calf_NEma)*(0.0466*calf_NEma^2)-0.1128)/calf_NEma)*DMIAF_temp*BI
  }
  #growing bulls and cows
  growing_DMI <- function(BW) {
    SBW <- 0.94*BW
    grow_NEma <- (SBW^0.75)*((0.077*(0.8+(CS - 1)*0.05))+(0.0007*(20-Temp)))
    DMI <- (SBW^0.75)*(((0.2435*grow_NEma)*(0.0466*grow_NEma^2)-0.0869)/grow_NEma)*DMIAF_temp*BI
  }
  #Lactating Cows
  lact_cow_DMI <- function(BW) {
    SBW <- 0.94*BW
    lact_NEma <- (SBW^0.75)*((0.077*(0.8+(CS - 1)*0.05)*1.2)+(0.0007*(20-Temp)))
    DMI <- (SBW^0.75)*(((0.04997*lact_NEma^2)+0.03840)/lact_NEma)*DMIAF_temp*BI+(0.2*MY)
  }
  #Pregnant (last 2/3's of pregnancy)
  dry_cow_DMI <- function(BW) {
    SBW <- 0.94*BW
    dry_NEma <- (SBW^0.75)*((0.077*(0.8+(CS - 1)*0.05))+(0.0007*(20-Temp)))
    DMI <- (SBW^0.75)*(((0.04997*dry_NEma)+0.04631)/dry_NEma)*DMIAF_temp*BI+(0.2*MY)
  }

  ##Excretion Calculations

  # Nitrogen excreted (Nennich et al. 2005)
  # Combination of cow and bull calves
  Prod.calf.N <- function() {
    wtCalf <- wtProd.bull.calf+wtProd.cow.calf
    nCalf <- nProd.bull.calf+nProd.cow.calf
    calf_DMI <- calf_DMI(wtCalf/nCalf)
    return((calf_DMI*calf_CP*DOF/6.25)-(41.2*(calf.max.wt-calf.initial.wt))+
             ((0.243*DOF*((calf.max.wt+calf.initial.wt)/2)^0.75)*((calf.max.wt-calf.initial.wt)/DOF)^1.097)*nCalf)
  }
  Prod.steer.grow.N <- function() {
    growing_DMI <- growing_DMI(wtProd.steer.grow/nProd.steer.grow)
    return((growing_DMI*growing_CP*DOF/6.25)-(41.2*(steer.max.wt-steer.initial.wt))+
             ((0.243*DOF*((steer.max.wt+steer.initial.wt)/2)^0.75)*((steer.max.wt-steer.initial.wt)/DOF)^1.097)*nProd.steer.grow)
  }
  Prod.cow.grow.N <- function() {
    growing_DMI <- growing_DMI(wtProd.cow.grow/nProd.cow.grow)
    return((growing_DMI*growing_CP*DOF/6.25)-(41.2*(steer.max.wt-steer.initial.wt))+
             ((0.243*DOF*((steer.max.wt+steer.initial.wt)/2)^0.75)*((steer.max.wt-steer.initial.wt)/DOF)^1.097)*nProd.steer.grow)
  }
  Breed.bull.grow.N <- function() {
    growing_DMI <- growing_DMI(wtBreed.bull.grow/nBreed.bull.grow)
    return((growing_DMI*growing_CP*DOF/6.25)-(41.2*(steer.max.wt-steer.initial.wt))+
             ((0.243*DOF*((steer.max.wt+steer.initial.wt)/2)^0.75)*((steer.max.wt-steer.initial.wt)/DOF)^1.097)*nProd.steer.grow)
  }
  Breed.cow.grow.N <- function() {
    growing_DMI <- growing_DMI(wtProd.cow.grow/nProd.cow.grow)
    return((growing_DMI*growing_CP*DOF/6.25)-(41.2*(steer.max.wt-steer.initial.wt))+
             ((0.243*DOF*((steer.max.wt+steer.initial.wt)/2)^0.75)*((steer.max.wt-steer.initial.wt)/DOF)^1.097)*nProd.steer.grow)
  }
  Breed.bull.mature.N <- function() {
    growing_DMI <- growing_DMI(wtProd.cow.grow/nProd.cow.grow)
    return((growing_DMI*growing_CP*DOF/6.25)-(41.2*(steer.max.wt-steer.initial.wt))+
             ((0.243*DOF*((steer.max.wt+steer.initial.wt)/2)^0.75)*((steer.max.wt-steer.initial.wt)/DOF)^1.097)*nProd.steer.grow)
  }
  #Use beef equation
  dry_N_excretion <- function() {
    dry_DMI <- dry_cow_DMI(wtDry/nDry)
    return((dry_DMI*dry_CP*78.39+51.4)*nDry)
  }

  ## Phosphorus Excretion
  calf_P_excretion <- function() {
    return(1)
  }
  growing_P_excretion <- function() {
    return(1)
  }
  lact_P_excretion <- function() {
    return(1)
  }
  dry_P_excretion <- function() {
    return(1)
  }

}
