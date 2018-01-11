


#############  layerS  ##################
if(animal == 'layer') {

  nChicks <- iChicks #Set starting count of young
  wtChicks <- 0.025*nChicks #each chick = 25g
  nlayers <- 0
  wtlayers <- 0

  # Flux rates
  kMature <- 1 #when chicks mature, they all do at once
  kBirth <- 1 #When chicks hatch, get all new chicks
  kCull <- 1 #when we sell, all sell
  kMortality <- kMortality*iChicks/switch_feed*2/100
  Temp <- if(Temp<=23) {23} else {if(Temp>31) {31} else{Temp}}

  layer_ADG <- function(Temp) {
    # # Use this for ADG calculations
    # # For use as BW in ADG eq.
    wt_per_bird <- sum(wtChicks,wtlayers)/sum(nChicks,nlayers)
    wt_per_bird <- wt_per_bird*1000

    #May et al 1998
    ADG <- -31.797 + (1.2071*Temp) + (0.21457*wt_per_bird) - (8.852E-5*wt_per_bird^2) +
      (1.51E-8*wt_per_bird^3) - (2.0772E-3*Temp*wt_per_bird)

    return(ADG/1000)
  }

  #Zuidhof et al 2014
  layer_Intake <- function(ADG) {
    I_ME <- (196*(sum(wtChicks,wtlayers)/sum(nChicks,nlayers))^0.7543)+2.4833*ADG*1000 #Convert to g
    ME <- if(nChicks > 0) {ME_young} else {ME_mature}
    Intake <- I_ME/ME
    return(Intake)
  }

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
