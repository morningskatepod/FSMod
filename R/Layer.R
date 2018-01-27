# Set Rates to 0
kMature <- 0
kSpent.Hens <- 0
kMolt <- 0
kBreed.eggs <- 0
kLaying.eggs <- 0
kLaying.spent <- 0
kVolitilization <- 0

# Set animal numbers to 0
nChicks.brown <- 0
nPullets.brown <- 0
nLaying.hens.brown <- 0
nBreed.hens.brown <- 0
nBreed.males.brown <- 0
nMolt.hens.brown <- 0
nChicks.white <- 0
nPullets.white <- 0
nLaying.hens.white <- 0
nBreed.hens.white <- 0
nBreed.males.white <- 0
nMolt.hens.white <- 0

# Set outputs to 0
Eggs <- 0
N <- 0
P <- 0

# Read input file
source('broiler.input.text')

iDay <- 1
nDay <- iDay

#############  layerS  ##################
if(animal == 'layer') {

  ## Start count
  nPullets.brown <- iPullets.brown
  nLaying.hens.brown <- iLaying.hens.brown
  nBreed.hens.brown <- iBreed.hens.brown
  nMolt.hens.brown <- iMolt.hens.brown
  nPullets.white <- iPullets.white
  nLaying.hens.white <- iLaying.hens.white
  nBreed.hens.white <- iBreed.hens.white
  nMolt.hens.white <- iMolt.hens.white
  wtPullets.brown <- 0
  wtPullets.white <- 0

  # Flux rates
  kMature <- 1 #when chicks mature, they all do at once
  kBirth <- 1 #When chicks hatch, get all new chicks
  kCull <- 1 #when we sell, all sell
  kMortality <- kMortality*iChicks/switch_feed*2/100
  Temp <- if(Temp<=23) {23} else {if(Temp>31) {31} else{Temp}}

### Daily Gain

  layer_ADG <- function(Temp) {
    # # Use this for ADG calculations
    # # For use as BW in ADG eq.
    wt_per_bird.brown <- sum(wtChicks,wtPullets.brown)/sum(nChicks,nPullets.brown)
    wt_per_bird.brown <- wt_per_bird.brown*1000
    wt_per_bird.white <- sum(wtChicks,wtPullets.white)/sum(nChicks,nPullets.white)
    wt_per_bird.white <- wt_per_bird.white*1000
    week <- round(nDay/7+1)

    if(nPullets.brown > 0) {
      Wm = 1.9143
      b = 0.14517
      t.star = 8.57678
      ADG <- Wm*b*(exp(-exp(-b*(week-t.star))))*exp(-b*(week-t.star))

    }
    if(nPullets.white > 0) {
      Wm = 1.50524
      b = 0.16687
      t.star = 7.54872
      ADG <- Wm*b*(exp(-exp(-b*(week-t.star))))*exp(-b*(week-t.star))
    }

    return(ADG/1000)
  }

### Dry Matter Intake

  #Zuidhof et al 2014
  layer_Intake <- function(ADG) {
    I_ME <- (196*(sum(wtChicks,wtlayers)/sum(nChicks,nlayers))^0.7543)+2.4833*ADG*1000 #Convert to g
    ME <- if(nChicks > 0) {ME_young} else {ME_mature}
    Intake <- I_ME/ME
    return(Intake)
  }


### Excretions

  layer_N_excretion <- function(ADG) {
    CP <- ifelse(nChicks>0, CPy,CPo)
    FI <- layer_Intake(ADG)
    # For Mature Birds
    if(nlayers > 0) {
      Nintake <- FI*1000 * nlayers * (CP/100)/6.25
      Nret <- 29 * (ADG*nlayers) # Constant 29 g/kg of BW gain according to ITAVI, 2013
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

  layer_P_excretion <- function(ADG) {
    nP <- ifelse(nChicks > 0, nP_young, nP_mature)
    P <- ifelse(nChicks > 0, P_young, P_mature)
    FI <- layer_Intake(ADG)
    P_exc <- exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  #Kornegay et al. 1996
                   ((0.4088*log(nP))^2)+((-0.0087*log(P))^2)+
                   (0.0012*log(P)*log(nP))) #g/kg intake
    Pexc <- FI*P_exc*sum(nlayers, nChicks)
    return(Pexc/1000) # Return P in kg
  }
}
