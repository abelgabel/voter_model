library(lattice)

# ################################################
# Parameters
# ################################################

mayority_model_dim2<-function(chosen_time,N, condition){
# t equals time
# s equals magnetization
s<-rep(0, chosen_time)

stoptime2<-0

# Number of simulations
# agents0  opinion of agents over time
agents0<-matrix(0,nrow=N^2, ncol=3)



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



x_axis<-rep(1,N)
for(i in 2:N)
{ x_axis <-c(x_axis,rep(i,N))}

y_axis <-seq(1,N,by=1)
for(i in 2:N)
{ y_axis <-c(y_axis,seq(1,N,by=1))}
agents0[,2]<-x_axis
agents0[,3]<-y_axis




 
# create plot
colmat<-matrix(agents0[,1], nrow=N ,ncol=N)
levelplot(colmat,col.regions=c('black','red'),xlab='',ylab='',colorkey=F, main='Initial Condition')
#dev.off()



# i is the time

i<-1
while(i<=chosen_time){
  # choose an agent at random
  x1<-sample(1:N,1)
  x2<-sample(1:N,1)
  
  # neighbourhood of the agent
  potential_choice<-agents0[abs(agents0[,2]-x1)+abs(agents0[,3]-x2)<=1 
                            & abs(agents0[,2]-x1)+abs(agents0[,3]-x2)>0,]
                            
# Choose neighbour                            
  choice<-sample(1:nrow(potential_choice),1)
  
  p<-1/4*(1-agents0[agents0[,2]==x1 &agents0[,3]==x2,1]*potential_choice[choice,1])
  
  agents0[agents0[,2]==x1 &agents0[,3]==x2,1]<-sample(c(agents0[agents0[,2]==x1 &agents0[,3]==x2,1] ,potential_choice[choice,1]),1,prob=c(1-p,p))
  
  # magnetization
  s[i]<-sum(agents0[,1])/N^2
  # if  |s|=1 stop loop.
  # The system has reached an absorbing state.  
  if(s[i]==1 | s[i]==-1){
    stoptime2<-i
    i<-t
    print(stoptime2)
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


