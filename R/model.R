setwd("C:/Users/dougl/Documents/PhD Papers/Farm Model")

#####################  FOR LOOP #######################
#Edit source of dairy file
source('FSMod/R/Dairy.R')

#Write an array
val <- array(1, dim = c(10, length(seq(iDay,fDay, by = 1))))
flow <- array(1, dim = c(9, length(seq(iDay,fDay, by = 1))))


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
    Pexc <- broiler_P_excretion(ADG)


    # Update Numbers of Animals
    nBroilers <- nBroilers+(maturing-culling)-(kMortality*(nBroilers > 0))
    nChicks <- nChicks+(born-maturing)-(kMortality*(nChicks > 0))
    meat_produced <- culling*(wtBroilers)

    wtBroilers <- (nBroilers > 0)*(wtBroilers + ADG*nBroilers + (t==1)*wtChicks)
    wtChicks <- (nChicks > 0)*(wtChicks + ADG*nChicks)
    if(t==2) {wtChicks <- 0.025*nChicks} #reset new chicks
  }

  if(animal == 'layer') {

    # Set t for broilers, know switch times
    t <- ifelse(nDay%%(switch_feed*2) == 0,2, ifelse(nDay%%switch_feed ==0,1,0))

    # Fluxes
    maturing <- (t==1)*(iChicks.brown+iChicks.white)
    new.layers <- (t==2)*(iPullets.brown+iPullets.white)
    molting <- (t==3)*(molt == 1)*(nLaying.hens.brown+nLaying.hens.white)

    # Updating Counts
    nPullets.brown <- nPullets.brown + (maturing - new.layers)*(nChicks.brown > 0)
    nLaying.hens.brown <- nLaying.hens.brown + (new.layers-molting)*(nLaying.hens.brown > 0)
    nBreed.hens.brown <- iBreed.hens.brown
    nMolt.hens.brown <- nMolt.hens.brown + (molting)
    nPullets.white <- iPullets.white + (maturing - new.layers)*(nChicks.white > 0)
    nLaying.hens.white <- iLaying.hens.white + (maturing - new.layers)*(nLaying.hens.white > 0)
    nBreed.hens.white <- iBreed.hens.white
    nMolt.hens.white <- nMolt.hens.white
  }

  if(animal == 'dairy') {
    born <- ((kFreshening*nDry*0.5)+(kFreshening*nHeifer.first.dry*0.5)+
               +(kMature*nCalf*0.5)+
      (kFreshening*nHeifer.second.dry*0.5)+(kFreshening*nHeifer.third.dry*0.5))*kMortality/100
    new.lact.herd <- (kFreshening*nDry)*(100-kMortality)/100
    new.first.lact <- kMature*nCalf*(100-kMortality)/100
    new.second.lact <- kFreshening*nHeifer.first.dry*(100-kMortality)/100
    new.third.lact <- kFreshening*nHeifer.second.dry*(100-kMortality)/100
    new.first.dry <- kDry*nHeifer.first.lact*(100-kMortality)/100
    new.second.dry <- kDry*nHeifer.second.lact*(100-kMortality)/100
    new.third.dry <- kDry*nHeifer.third.lact*(100-kMortality)/100
    new.dry.herd <- kDry*nLact*(100-kMortality)/100
    new.lact.from.third <- kFreshening*nHeifer.third.dry*(100-kMortality)/100
    culling <- (kCull/100*nLact)/365

    # Excretions
    Nexc <- calf.N()+heifer.first.lact.N()+heifer.second.lact.N()+heifer.third.lact.N()+
      heifer.first.dry.N()+heifer.second.dry.N()+heifer.third.dry.N()+cow.lact.N()+cow.dry.N()
    Pexc <- calf.P()+heifer.first.lact.P()+heifer.second.lact.P()+heifer.third.lact.P()+
      heifer.first.dry.P()+heifer.second.dry.P()+heifer.third.dry.P()+cow.lact.P()+cow.dry.P()

    #Update Numbers of Animals
    nCalf <- nCalf + born - new.first.lact + 1
    nHeifer.first.dry <- nHeifer.first.dry + new.first.dry - new.second.lact
    nHeifer.second.dry <- nHeifer.second.dry + new.second.dry - new.third.lact
    nHeifer.third.dry <- nHeifer.third.dry + new.third.dry - new.lact.from.third
    nHeifer.first.lact <- nHeifer.first.lact + new.first.lact - new.first.dry
    nHeifer.second.lact <- nHeifer.second.lact + new.second.lact - new.second.dry
    nHeifer.third.lact <- nHeifer.third.lact + new.third.lact - new.third.dry
    nLact <- nLact + new.lact.herd - culling - new.dry.herd + new.lact.from.third
    nDry <- nDry + new.dry.herd - new.lact.herd
    nCalf <- ifelse(nCalf < 0, 0, nCalf)

    meat_produced <- culling*(wtLact)
  }

  if(animal == 'beef') {

    calves <- kCow.birth * nCow.dry # same as new lactating animals
    lactating <- kCow.birth * nCow.dry
    bred.cows <- kCow.bred * nCow.lact
    dry.cows <- kCow.dry * nCow.bred
    slaugher.lact.cows <- kCow.slaughter * nCow.lact

    growing.steers <- kProd.steer.grow * nProd.bull.calf
    growing.cows <- kProd.cow.grow * nProd.cow.calf
    finishing.steers <- kProd.steer.finish * nProd.steer.grow
    finishing.cows <- kProd.cow.finish * nProd.cow.grow
    slaughter.steers <- kProd.slaughter * nProd.steer.finish
    slaughter.cows <- kProd.slaughter * nProd.cow.finish

    growing.bred.cows <- kBreed.cow.grow * nBreed.cow.calf
    growing.bred.bulls <- kBreed.bull.grow * nBreed.bull.calf
    mature.bred.bulls <- kBreed.cow.mature * nBreed.bull.grow
    mature.bred.cows <- kBreed.bull.mature * nBreed.cow.grow
    slaughter.bred.bulls <- kBreed.bull.slaughter * nBreed.bull.mature

    # Excretions
    Nexc <- calf.N()+heifer.first.lact.N()+heifer.second.lact.N()+heifer.third.lact.N()+
      heifer.first.dry.N()+heifer.second.dry.N()+heifer.third.dry.N()+cow.lact.N()+cow.dry.N()
    Pexc <- calf.P()+heifer.first.lact.P()+heifer.second.lact.P()+heifer.third.lact.P()+
      heifer.first.dry.P()+heifer.second.dry.P()+heifer.third.dry.P()+cow.lact.P()+cow.dry.P()

    # Update Numbers of Animals
    nCow.bred <- nCow.bred + bred.cows - dry.cows - slaughter.lact.cows
    nCow.dry <- nCow.dry + dry.cows + mature.bred.cows - lactating
    nCow.lact <- nCow.lact + lactating - mature.bred.cows

    nProd.bull.calf <- nProd.bull.calf + (calves*male.birth.rate) - growing.steers
    nProd.cow.calf <- nProd.cow.calf + (calves*(1-male.birth.rate)) - growing.cows
    nProd.steer.grow <- nProd.steer.grow + growing.steers - finishing.steers
    nProd.cow.grow <- nProd.cow.grow + growing.cows - finishing.cows
    nProd.steer.finish <- nProd.steer.finish + finishing.steer - slaughter.steer
    nProd.cow.finish <- nProd.cow.finish + finishing.cows - slaughter.cows

    nBreed.cow.calf <- nBreed.cow.calf + (calves*(1-male.birth.rate)*breedstock.rate) - growing.bred.cows
    nBreed.bull.calf <- nBreed.bull.calf + (calves*(male.birth.rate)*breedstock.rate) - growing.bred.bulls
    nBreed.cow.grow <- nBreed.cow.grow + growing.bred.cows - mature.bred.cows
    nBreed.bull.grow <- nBreed.bull.grow + growing.bred.bulls - mature.bred.bulls
    nBreed.bull.mature <- nBreed.bull.mature + mature.bred.bulls - slaughter.bred.bulls

    meat_produced <- sum((slaughter.bred.bulls*breed.bull.wt),(slaughter.lact.cows*lact.cow.wt),
                         (slaughter.cows*cow.max.wt),(slaughter.bulls*steer.max.wt))
  }

  # New Totals
  Meat <- Meat + meat_produced
  # Eggs <- Eggs + laying*nLayers
  # Milk <- Milk + kMilk*nLact
  N <- N + Nexc
  P <- P + Pexc
  P.day <- Pexc

  # Update Numbers of Animals
  flow[1,nDay] <- nCalf
  flow[2,nDay] <- nHeifer.first.lact
  flow[3,nDay] <- nHeifer.first.dry
  flow[4,nDay] <- nLact
  flow[5,nDay] <- nDry
  flow[6,nDay] <- nHeifer.second.lact
  flow[7,nDay] <- nHeifer.third.dry
  flow[8,nDay] <- nHeifer.third.lact
  flow[9,nDay] <- nHeifer.second.dry

  # Update values
  val[1,nDay] <- nCalf
  val[2,nDay] <- nHeifer.third.dry
  val[3,nDay] <- nLact
  val[4,nDay] <- nDry
  val[5,nDay] <- total.parlor
  val[6,nDay] <- total.drypen
  val[7,nDay] <- Meat
  # val[8,nDay] <- ADG*1000
  val[9,nDay] <- Nexc
  val[10,nDay] <- P

  nDay = nDay+1
}
##############################################

# Plot results
par(mfrow = c(2,5))
plot(flow[1,], main = 'Calves', xlab = 'Day', ylab = 'Count')
plot(flow[2,], main = '1st Lact Heifers', xlab = 'Day', ylab = 'Count')
plot(flow[3,], main = '1st Dry Heifers', xlab = 'Day', ylab = 'Count')
plot(flow[6,], main = '2nd Lact Heifers', xlab = 'Day', ylab = 'Count')
plot(flow[9,], main = "2nd Dry Heifers", xlab = 'Day', ylab = 'Count')
plot(flow[8,], main = "3rd Lact Heifers", xlab = 'Day', ylab = 'Count')
plot(flow[7,], main = '3rd Dry Heifers', xlab = 'Day', ylab = 'Count')
plot(flow[4,], main = 'Lact Cows', xlab = 'Day', ylab = 'Count')
plot(flow[5,], main = 'Dry Cows', xlab = 'Day', ylab = 'Count')

# plot(val[10,], main = "P Excretion", xlab = 'Day', ylab = 'Wt, kg')
