#define function
#define starting number, growth rate, carrying capacity, timesteps
popSim<-function(N0= 1,r=0.1,K=1000000,timesteps=1000){
  Ns<-numeric(timesteps)
  Ns[1]<-N0
  for(t in 2:timesteps){
    Ns[t]<-Ns[t-1]+Ns[t-1]*r*(1-Ns[t-1]/K)
  }
  return(Ns)
}

#run simulation twice
#first run = normal
#second run shows drug treatment after 100 cells have grown (r becomes -0.1)
sim1=popSim()
sim2=popSim(N0=100,r=-0.1)

#create dataframe with info from both simulations
Ns2<-data.frame(time=c(1:timesteps,1:timesteps),N=c(sim1,sim2),sim=rep(c("without drug","with drug"),each=timesteps))

# plot simulations
library(ggplot2)
ggplot(data=Ns2,aes(x=time,y=N,color=sim)) + 
  geom_line() +
  theme_classic()