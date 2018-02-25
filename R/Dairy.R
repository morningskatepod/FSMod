# setwd("C:/Users/dougl/Documents/PhD Papers/004 - Farm Model")
############################################

# Set all rates to 0 before reading files
kCull <- 0
kMature <- 1/365*2
kDry <- 1/305
kFreshening <- 1/60
kMilk <- 0
kMortality <- 7 #as whole number percentage

# Set outputs to 0
Meat <- 0
Milk <- 0
N <- 0
P <- 0

# Set animal numbers to 0
nCalf <- 0
nHeifer.first.lact <- 0
nHeifer.second.lact <- 0
nHeifer.third.lact <- 0
nHeifer.first.dry <- 0
nHeifer.second.dry <- 0
nHeifer.third.dry <- 0
nLact <- 0
nDry <- 0

wtCalf <- 0
wtHeifer.first.lact <- 0
wtHeifer.second.lact <- 0
wtHEifer.third.lact <- 0
wtHeifer.first.dry <- 0
wtHeifer.second.dry <- 0
wtHeifer.third.dry <- 0
wtLact <- 0
wtDry <- 0

# Read input file
source('dairy.input.text')

iDay <- 1
nDay <- iDay

##################  DAIRY  #####################

if(animal == 'dairy') {

  #Start counts
  nCalf <- iCalf #Set starting count of young
  nHeifer.first.lact <- iHeifer.first.lact
  nHeifer.first.dry <- iHeifer.first.dry
  nHeifer.second.lact <- iHeifer.second.lact
  nHeifer.second.dry <- iHeifer.second.dry
  nHeifer.third.lact <- iHeifer.third.lact
  nHeifer.third.dry <- iHeifer.third.dry
  nLact <- iLact
  nDry <- iDry

  #Return adg as function of time for female cows 0 - 2500 days
  dairy_ADG <- function(t) { #From Perotto et al, 1992
    A <- 619 #asymptotic weight, kg
    k <- 0.0020 #Rate parameter
    b <- 0.905 # integration constant
    M <- 1.2386 # inflection parameter
    # return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
    return(A*(1-(b*exp(-k*t)))^M)
  }

  # #Determining average weights for each group per animal
  # wtCalf <- integrate(dairy_ADG, lower = 0, upper  = days.to.first.calf)[[1]]/days.to.first.calf
  # wtHeifer.first.lact <- integrate(dairy_ADG, lower = days.to.first.calf, upper  = days.to.first.calf+305)[[1]]/305
  # wtHeifer.second.lact <- integrate(dairy_ADG, lower = days.to.first.calf+365, upper  = days.to.first.calf+365+305)[[1]]/305
  # wtHeifer.third.lact <- integrate(dairy_ADG, lower = days.to.first.calf+365+365, upper  = days.to.first.calf+365+365+305)[[1]]/305
  # wtHeifer.first.dry <- integrate(dairy_ADG, lower = days.to.first.calf+305, upper  = days.to.first.calf+365)[[1]]/60
  # wtHeifer.second.dry <- integrate(dairy_ADG, lower = days.to.first.calf+365+305, upper  = days.to.first.calf+365+365)[[1]]/60
  # wtHeifer.third.dry <- integrate(dairy_ADG, lower = days.to.first.calf+365+365+305, upper  = days.to.first.calf+365*3)[[1]]/60
  # wtLact <- dairy_ADG(2500)
  # wtDry <- dairy_ADG(2500)

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
  }
  #Yearlings
  heifer_DMI <- function(BW) {
    SBW <- 0.94*BW
    DMI <- (SBW^0.75)*(((0.2435*heifer_NEma)*(0.0466*heifer_NEma^2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
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

####################### Excretion Calculations ############

  # Nitrogen excreted (Nennich et al. 2005)
  calf.N <- function() {
    calf_DMI <- calf_DMI(wtCalf)
    return((calf_DMI*calf_CP*112.55)*nCalf)
  }

  heifer.first.lact.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.first.lact)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.first.lact)
  }

  heifer.second.lact.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.second.lact)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.second.lact)
  }

  heifer.third.lact.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.third.lact)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.third.lact)
  }

  heifer.first.dry.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.first.dry)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.first.dry)
  }

  heifer.second.dry.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.second.dry)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.second.dry)
  }

  heifer.third.dry.N <- function() {
    heifer_DMI <- heifer_DMI(wtHeifer.third.dry)
    return((heifer_DMI*heifer_CP*78.39+51.4)*nHeifer.third.dry)
  }

  cow.lact.N<- function() {
    lact_DMI <- lact_cow_DMI(wtLact/nLact)
    return(((lact_DMI*lact_CP*84.1)+(wtLact/nLact*0.196))*nLact)
  }

  #Use beef equation
  cow.dry.N <- function() {
    dry_DMI <- dry_cow_DMI(wtDry)
    return((dry_DMI*dry_CP*78.39+51.4)*nDry)
  }



  # Phosphorus excreted
  calf.P <- function() {
    calf_DMI <- calf_DMI(wtCalf)
    return((calf_DMI*calf_CP*112.55)*nCalf)
  }

  cow.lact.P<- function() {
    DMI <- lact_cow_DMI(wtLact/nLact)
    return(((lact_DMI*lact_P)-(2*(wtLact/nLact)/1000)-0.02743*
              exp(((0.05527-0.000075*DIM)*DIM))-
            0.02743*exp(((0.05527-0.000075*(DIM-1))*(DIM-1)))*
            (1.2+4.635*MW^0.22*(wtLact/nLact)^-0.22)*ADG/0.96)*nLact)
  }

  #Use beef equation
  cow.dry.P <- function() {
    dry_DMI <- dry_cow_DMI(wtDry)
    return((dry_DMI*dry_CP*78.39+51.4)*nDry)
  }

}
