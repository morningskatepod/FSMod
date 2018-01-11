setwd("C:/Users/dougl/Documents/PhD Papers/Farm Model")

#####################  FOR LOOP #######################
source('FSMod/R/Dairy.R')

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
    Pexc <- broiler_P_excretion(ADG)


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

    # Excretions
    Nexc <- calf_N_excretion()+heifer_N_excretion()+lact_N_excretion()+dry_N_excretion()
    Pexc <- calf_P_excretion()+heifer_P_excretion()+lact_P_excretion()+dry_P_excretion()

    #Update Numbers of Animals
    nCalf <- nCalf #+ born
    nHeifer <- nHeifer #+ maturing
    nLact <- nLact #+ freshening
    nDry <- nDry #+ drying
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
    Nexc <- calf_N_excretion()+growing_N_excretion()+lact_N_excretion()+dry_N_excretion()
    Pexc <- calf_P_excretion()+growing_P_excretion()+lact_P_excretion()+dry_P_excretion()

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



  # Update values
  val[1,nDay] <- nCalf
  val[2,nDay] <- nHeifer
  val[3,nDay] <- nLact
  val[4,nDay] <- nDry
  val[5,nDay] <- Pexc
  val[6,nDay] <- nBroilers+nChicks
  val[7,nDay] <- Meat
  # val[8,nDay] <- ADG*1000
  val[9,nDay] <- Nexc
  val[10,nDay] <- P

  nDay = nDay+1
}
##############################################
# results <- rbind(results, c(Meat,N,P))


# Plot results
par(mfrow = c(2,5))
plot(val[1,], main = 'Calves', xlab = 'Day', ylab = 'Count')
plot(val[2,], main = 'Heifers', xlab = 'Day', ylab = 'Count')
plot(val[3,], main = 'Lactating Cows', xlab = 'Day', ylab = 'Count')
plot(val[4,], main = 'Dry Cows', xlab = 'Day', ylab = 'Count')
plot(val[5,], main = 'P per day', xlab = 'Day', ylab = 'kg, d')
plot(val[6,], main = 'Broiler Numbers', xlab = 'Day', ylab = 'Count')
plot(val[7,], main = 'Meat', xlab = 'Day', ylab = 'Wt, kg')
plot(val[8,], main = "ADG", xlab = 'Day', ylab = 'g/d')
plot(val[9,], main = "N Excretion", xlab = 'Day', ylab = 'N, kg')
plot(val[10,], main = "P Excretion", xlab = 'Day', ylab = 'Wt, kg')
#
#
# ggplot(results, aes(cull, value, label = round(value,2), group = variable)) +
#   geom_bar(aes(fill = variable), color = 'black', position = 'dodge', stat = 'identity') +
#   xlab("Mortality Rate, %") + ylab("Increase in Production")+
#   scale_y_continuous(labels = percent) +
#   geom_text(size = 4, position = position_dodge(width = 0.9), vjust = -0.5) +
#   theme(legend.title = element_blank()) +
#   ggtitle('Production of Meat, Excreted N and P')+
#   theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

# results <- readRDS('n_exc.RDS')
