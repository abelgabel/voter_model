
# ################################################
# Load library
# ################################################
library(lattice)

# ################################################
# Function
# ################################################

mayority_model_dim2<-function(chosen_time,N, condition){

# s equals magnetization
s<-rep(0, chosen_time)
# Time when system reaches an absorbing state 
stoptime2<-0

# Number of simulations
# agents0  opinion of agents over time
agents0<-matrix(0,nrow=N^2, ncol=3)




# Compute coordinates of agents
x_axis<-rep(1,N)
for(i in 2:N)
{ x_axis <-c(x_axis,rep(i,N))}

y_axis <-seq(1,N,by=1)
for(i in 2:N)
{ y_axis <-c(y_axis,seq(1,N,by=1))}
# Agents cordinate are in column 2 and 3
agents0[,2]<-x_axis
agents0[,3]<-y_axis

# ################################################
# Initial Condition
# ################################################
# Random Initial condition
# First row from matrix agents0 represents agents opinion (-1 or 1).

# Randomised opinion:
if(condition=='random'){
init0<-2*(rbinom(N^2,1,1/2)-1/2)
agents0[,1]<-init0
}
# Boundary condition:
else{
agents0[agents0[,2]<=2 | agents0[,3]<=2 | agents0[,2]>=N-2 |agents0[,3]-1>=N-2,1]<-1
}





 
# Create plot:
# Color black: opinon -1, color red: opinion 1
colmat<-matrix(agents0[,1], nrow=N ,ncol=N)
print(levelplot(colmat,col.regions=c('black','red'),xlab='',ylab='',colorkey=F, main='Initial Condition'))



# i is the time
#  Simulations start at period 1
i<-1
# Simulation finished at period chosen_time
while(i<=chosen_time){
  # Choose an agent at random
  x1<-sample(1:N,1)
  x2<-sample(1:N,1)
  
  # Neighbourhood of the agent
  potential_choice<-agents0[abs(agents0[,2]-x1)+abs(agents0[,3]-x2)<=1 
                            & abs(agents0[,2]-x1)+abs(agents0[,3]-x2)>0,]
                            
# Choose neighbour                            
  choice<-sample(1:nrow(potential_choice),1)
 # Probability that agent is influenced by its neighbour 
  p<-1/4*(1-agents0[agents0[,2]==x1 &agents0[,3]==x2,1]*potential_choice[choice,1])
  
  agents0[agents0[,2]==x1 &agents0[,3]==x2,1]<-sample(c(agents0[agents0[,2]==x1 &agents0[,3]==x2,1] ,potential_choice[choice,1]),1,prob=c(1-p,p))
  
  # Magnetization
  s[i]<-sum(agents0[,1])/N^2
  # If  |s|=1 stop loop.
  # The system has reached an absorbing state.  
  if(s[i]==1 | s[i]==-1){
    stoptime2<-i
    i<-t
    # Print when state reaches equilibrium
    print(stoptime2)
    # Stop simulations
    break
  }
  # plot only certain steps   
  if(i%%100==1){
  	colmat<-matrix(agents0[,1], nrow=N ,ncol=N)
  	print(levelplot(colmat,col.regions=c('black','red'),ylab='',colorkey=F ,main=list(paste0('Period: ',i),side=1,line=0.5),xlab=''))
 
  }
  i<-i+1
}

}


