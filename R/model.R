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
  P <- P + Pexc

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
  val[10,nDay] <- P

  nDay = nDay+1
}
##############################################
results <- rbind(results, c(Meat,N,P))
}

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


ggplot(results, aes(cull, value, label = round(value,2), group = variable)) +
  geom_bar(aes(fill = variable), color = 'black', position = 'dodge', stat = 'identity') +
  xlab("Mortality Rate, %") + ylab("Increase in Production")+
  scale_y_continuous(labels = percent) +
  geom_text(size = 4, position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(legend.title = element_blank()) +
  ggtitle('Production of Meat, Excreted N and P')+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

results <- readRDS('n_exc.RDS')
