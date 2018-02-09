# Set Rates to 0
kMature <- 0
kSpent.Hens <- 0
kMolt <- 0
kBreed.eggs <- 0
kLaying.eggs <- 0
kLaying.spent <- 0
kMortality <- 0
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
Meat <- 0

# Read input file
source('layer.input.text')

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
  Temp <- if(Temp<=23) {23} else {if(Temp>31) {31} else{Temp}}

### Daily Gain

  layer_ADG <- function() {
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
### Total N needed from SID Lys requirement
  Total.N.req <- function() {
    W <- (sum(wtChicks,wtlayers)/sum(nChicks,nlayers))^0.75
    ADG <- layer_ADG()
    Egg.Mass <- 56
    SID.Lys <- 0.07*W + 0.02*ADG + 0.0124*Egg.Mass
    ME.int <- (196*(sum(wtChicks,wtlayers)/sum(nChicks,nlayers))^0.7543)+2.4833*ADG*1000 #Convert to g
    DMI <- 1/(2900/ME.int)*1000 # in grams
    dig.lys <- SID.Lys/DMI
    N.req <- SID.Lys*1*0.1919 +
      SID.Lys*0.53*0.0939 +
      SID.Lys*0.80*0.1176 +
      SID.Lys*0.23*0.1372 +
      SID.Lys*0.96*0.3216 +
      SID.Lys*0.80*0.1599 +
      SID.Lys*0.93*0.1196 +
      SID.Lys*0.78*0.1068 +
      SID.Lys*1.19*0.1068 +
      SID.Lys*0.28*0.2708 +
      SID.Lys*0.63*0.0848
    Total.N <- N.req/0.44*6.25
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
      Nret <- Total.N.req
      Nexc <- Nintake - Nret
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
