

library(readr)
library(readxl)
library(dplyr)

##################################################################################################

#First we intialize all the functions used throughout each simulation:

##################################################################################################
# FUNCTIONS

#These are the functions used to calculate perceived benefit after t=0

#i = mgmt level of grower j *as character string*
#j = grower of focus
#t = time by months; time starts at 0; one year is months 0-11
#k = connected grower of interest

#NOTE: the NODE is the growers number i.e. grower #10. the ROW is the matrix row associated with the 
#      NODE at a specific time i.e. grower (NODE) #10 at time t=1 may be ROW 1142

#function
# 1 if grower j and k and are linked in comm.net
# read in NODE number of interest:
mgmtlinks<-function(j,k){
  if(comm.net[j,k]==1){
    return(1)
  }
  else{
    return(0)
  }
} 

#function
# 1 if grower k practices management i at time t 
# read in NODE number of interest:
mgmtlevel<-function(i,k,t){
  if(avocado.groves[k+size*t,3]==i){
    return(1)
  }
  else{
    return(0)
  }
} 

#function
# counts connections of j with management i at time t
# read in NODE of interest, with i as character string, and time of interest:
mgmtconnect<-function(i,j,t){
  count<-0
  for (w in 1:size){
    count<-count+(mgmtlevel(i,w,t)*mgmtlinks(j,w)) #mgmtlevel function lines 34-41; mgmtlinks function lines 23-30
  }
  return(count)
}

#function
# returns perceived benefit grower j has of other farmers
# k who practice management i:
grovebenefit<-function(i,j,t){
  av<-0
  for(w in 1:size){
    av<-av+(avocado.groves[[w+size*t,5]]*mgmtlevel(i,w,t)*mgmtlinks(j,w)) #mgmtlevel function lines 34-41; mgmtlinks function lines 23-30
  }
  gb<-av/(mgmtconnect(i,j,t)) #mgmtconnect function lines 45-51
  if(is.finite(gb)==TRUE){
    return(gb)
  }
  else{
    return(0)
  }
}

#function
# cumulative percieved benefit for management i or grower j at time t
# C_ijt = stubbornness * C_ij (t-1) + (1-stubbornness) * [stubbornness * perception_of_management + (1-stubbornness) * grovebenefit]
# the break down of this is in the document: 
cumperbenefit<-function(i,j,t){
  if(i=="low"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),7]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*(t-1),7]+(1-avocado.groves[j,6])*grovebenefit(i,j,(t-1)))
  }
  if(i=="medium"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),8]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*(t-1),8]+(1-avocado.groves[j,6])*grovebenefit(i,j,(t-1)))
  }
  if(i=="high"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),9]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*(t-1),9]+(1-avocado.groves[j,6])*grovebenefit(i,j,(t-1)))
  }
  return(c.ben)
} #grovebenefit function lines 55-67

#########################################################################################################
#These are the functions used for moving through time

#function
# calculating within-grove spread:
how.much.disease.is.there.within.grove<-function(N){
  if(avocado.groves[N-size,10]==1){ #if disease is present at previous time step
    if(avocado.groves[N-size,3]=="low"){  #if the management strategy is low
      s<-sample(c(0,1),1,prob=c(0.05,0.95)) #5% chance of stopping disease
      if(s==1){ #if unsuccessful management
        pathogen<-sample(16:24,1) #new diseased trees increases by 16 and 24 trees
      }
      else{ #if successful management
        pathogen<-0 #there is no new disease
      }
    }
    if(avocado.groves[N-size,3]=="medium"){  #if the management strategy is medium
      s<-sample(c(0,1),1,prob=c(0.4,0.6)) #40% chance of stopping disease
      if(s==1){ #if unsuccessful management
        pathogen<-sample(4:8,1) #new diseased trees increases by 4 to 8 trees
      }
      else{ #if successful management
        pathogen<-0 #there is no new disease
      }
    }
    if(avocado.groves[N-size,3]=="high"){   #if the management strategy is high
      s<-sample(c(0,1),1,prob = c(0.9,0.1)) #90% chance of stopping disease
      if(s==1){ #if unsuccessful management
        pathogen<-sample(1:4,1) #new diseased trees increases by 0-4 trees
      }
      else{ #if successful management
        pathogen<-0 #there is no new disease
      }
    }
    new.disease<-avocado.groves[[N-size,11]]+pathogen #new.disease=old disease + new growth from functions above
    return(new.disease)
  }
  else{ #if theres no disease at previous time step
    return(0) #there is no disease
  }
} 

#function
# how many healthy trees are there in a grove:
how.many.healthy.trees.are.there<-function(N){
  if(avocado.groves[N-size,10]==1){ #if there is disease in the grove 
    health<-(avocado.groves[[N-size,12]]-avocado.groves[[N,11]]) #healthy trees= 100 trees per acre - diseased trees(from function above)
    return(health)
  }
  else{ #if there isn't disease
    health<-round(avocado.groves[[N-size,4]]*100) #the healthy trees remain the same
    return(health)
  }
} 

#function 
# calculating monthly compounding costs:
how.much.did.this.cost.monthly<-function(N){
  if(avocado.groves[N-size,3]=="low"){ #if you practice no management 
    money<-avocado.groves[[N-size,13]] 
  } #costs(t+1)=costs(t)
  else if(avocado.groves[N-size,3]=="medium"){ #if you practice medium management
    money<-(avocado.groves[[N-size,12]])*cost.per.tree.health.med+avocado.groves[[N-size,11]]*cost.per.tree.sick.medium+avocado.groves[[N-size,13]]
  } #costs(t+1)=healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t)
  else if(avocado.groves[N-size,3]=="high"){
    money<-(avocado.groves[[N-size,12]])*cost.per.tree.health.high+avocado.groves[[N-size,11]]*cost.per.tree.sick.high+avocado.groves[[N-size,13]]
  } #costs(t+1)=healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t)
  else if(avocado.groves[N-size,3]=="dead"){ #if your grove has end-gamed itself
    money<-0
  } #you make no money
  return(money)
} 

#function 
# calculating the new yearly costs:
how.much.did.this.cost.newyear<-function(N){
  if(avocado.groves[N-size,3]=="low"){ #if you practice no management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+avocado.groves[[N-size,13]]+stick(N)
  } # costs(t+1)=sick.trees(t)*yearly.loss.from.yield + costs(t)
  else if(avocado.groves[N-size,3]=="medium"){ #if you practice medium management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+(avocado.groves[[N-size,12]])*cost.per.tree.health.med+avocado.groves[[N-size,11]]*cost.per.tree.sick.medium+avocado.groves[[N-size,13]]
  } # costs(t+1)=healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t) + sick.trees(t)*yearly.loss.from.yield
  else if(avocado.groves[N-size,3]=="high"){ #if you practice high management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+(avocado.groves[[N-size,12]])*cost.per.tree.health.high+avocado.groves[[N-size,11]]*cost.per.tree.sick.high+avocado.groves[[N-size,13]]
  } # healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t) + sick.trees(t)*yearly.loss.from.yield
  else if(avocado.groves[N-size,3]=="dead"){ #if grove is ded
    money<-0
  } # you make no money
  return(money)
}

#function
# determining if diseased has spread to a grove:
is.there.disease.here<-function(N,t){
  if(avocado.groves[N-size,10]==1){ #if there's already disease here...
    return(1) #there's still disease here
  }
  else{
    for(i in 1:size){ #otherwise...
      if(avocado.groves[i+size*(t-1),10]==1 && bp.net(N-size*t,i)==1){ #bp.net function lines 551-555
        return(1) #if there is disease at grove i, and the biophysical network returns i, there will be movement
      }
    }
    return(0) #otherwise no disease spread
  }
}

#function 
# for filling in data by the month
data.fill.monthly<-function(N,t){
  cash<-carrot(N) #carrot function lines 287-303
  avocado.groves[N,1]<<-t
  avocado.groves[N,2]<<-avocado.groves[N-size,2]
  avocado.groves[N,3]<<-avocado.groves[N-size,3]
  avocado.groves[N,4]<<-avocado.groves[N-size,4]
  avocado.groves[N,5]<<-avocado.groves[N-size,5]
  avocado.groves[N,6]<<-avocado.groves[N-size,6]
  avocado.groves[N,7]<<-avocado.groves[N-size,7]
  avocado.groves[N,8]<<-avocado.groves[N-size,8]
  avocado.groves[N,9]<<-avocado.groves[N-size,9]
  avocado.groves[N,10]<<-is.there.disease.here(N,t) #is.there.disease.here function lines 174-186
  avocado.groves[N,11]<<-how.much.disease.is.there.within.grove(N) #how.much.disease.is.there.within.grove function lines 90-125
  avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N) #how.many.heatlhy.trees.are.there function lines 128-137
  avocado.groves[N,13]<<-cash #from above line 191
# 
  if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){
    avocado.groves[N,3]<<-"dead"
    avocado.groves[N,5]<<--1
    avocado.groves[N,10]<<--1
    avocado.groves[N,11]<<--1
    avocado.groves[N,12]<<--1
    avocado.groves[N,13]<<--1
  }
}

#function 
# for filling in data for the turn of the new year: 
data.fill.time12<-function(N,t){
  # nested function
  perc.not<-cumperbenefit("low",N-size*t,t-1) #cumperbenefit function lines 72-83
  perc.stu<-cumperbenefit("medium",N-size*t,t-1) #cumperbenefit function lines 72-83
  perc.fun<-cumperbenefit("high",N-size*t,t-1) #cumperbenefit function lines 72-83
  
  if(perc.not>perc.fun && perc.not>perc.stu){
    man<-"low"
  }
  if(perc.stu>perc.fun && perc.stu>perc.not){
    man<-"medium"
  }
  if(perc.fun>perc.not && perc.fun>perc.stu){
    man<-"high"
  }
  # nested function
  cash<-how.much.did.this.cost.newyear(N) #how.muc.did.this.cost.new.year function lines 157-171
  # populating groves
  avocado.groves[N,1]<<-t
  avocado.groves[N,2]<<-avocado.groves[N-size*t,2]
  avocado.groves[N,3]<<-man
  avocado.groves[N,4]<<-avocado.groves[N-size*t,4]
  avocado.groves[N,5]<<-avocado.groves[N-size,12]*51.22-cash #cash from above line 234
  avocado.groves[N,6]<<-avocado.groves[N-size*t,6]
  avocado.groves[N,7]<<-perc.not
  avocado.groves[N,8]<<-perc.stu
  avocado.groves[N,9]<<-perc.fun
  avocado.groves[N,10]<<-is.there.disease.here(N,t) #is.there.disease.here function lines 174-186
  avocado.groves[N,11]<<-0
  avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N) #how.many.heatlhy.trees.are.there function lines 128-137
  avocado.groves[N,13]<<-0
  
  if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){
    avocado.groves[N,3]<<-"dead"
    avocado.groves[N,5]<<--1
    avocado.groves[N,10]<<--1
    avocado.groves[N,11]<<--1
    avocado.groves[N,12]<<--1
    avocado.groves[N,13]<<--1
  }
}
####################################################################################################################

#Carrots and Sticks

#I am still working on this bit
#So ignore this for now

#function
# stick statement 
stick<-function(N){
  if(avocado.groves[N-size,3]=="high" || avocado.groves[N-size,3]=="medium"){
    return(0)
  }
  else{
    s<-sample(c(0,1),1,prob=c(1-pcos,pcos))
    if(s==0){
      return(0)
    }
    else{
      if(avocado.groves[N-size,10]==1){
        mny<-avocado.groves[[N-size,5]]*(stick.perc.increase/100)
        return(mny)
      }
      else{
        return(0)
      }
    }
  }
}

#function
# carrot statement
carrot<-function(N){
  if(avocado.groves[N-size,3]=="low"){
    money<-how.much.did.this.cost.monthly(N) #how.much.did.this.cost.monthly function lines 140-154
    return(money)
  }
  else{
    if(money.pot<=0){
      money<-how.much.did.this.cost.monthly(N) #how.much.did.this.cost.monthly function lines 140-154
      return(money)
    }
    else{
      money.pot<<-money.pot-how.much.did.this.cost.monthly(N)*0.5 #how.much.did.this.cost.monthly function lines 140-154
      costs.covered<-how.much.did.this.cost.monthly(N)*0.5 #how.much.did.this.cost.monthly function lines 140-154
      return(costs.covered)
    }
  }
}

####################################################################################################################
####################################################################################################################
####################################################################################################################
#Putting everything together

#function 
# for wrapping everything together (entire modeling)
the.entire.process<-function(size.of.mat,t){   #read in the size of the matrix and the number of MONTHS you'd like
  size.of.mat
  timestep<-1
  while(timestep<=t){
    if(timestep%%12!=0){
      for(N in 1:size.of.mat){
        data.fill.monthly(N+size.of.mat*timestep,timestep) #data.fill.monthly function lines 189-215 
      }
    }
    if(timestep%%12==0){
      for(N in 1:size.of.mat){
        data.fill.time12(N+size.of.mat*timestep,timestep) #data.fill.time12 function lines 218-258
      }
    }
    timestep=timestep+1
  }
  
  #
  
  #sum(avocado.groves[0:size,12])
  final.trees<-0
  for(i in 1:size.of.mat){
    if(avocado.groves[size.of.mat*120+i,12]==-1){
      final.trees=final.trees+0
    }
    else{
      final.trees=final.trees+avocado.groves[[size.of.mat*120+i,12]]
    }
  }
jazz<-(final.trees/sum(avocado.groves[0:size.of.mat,12]))*100
  
#calculate the total of each strat at the beginning and end of the simulation
carl<-matrix(0,nrow=10,ncol=4)
colnames(carl)<-c("low","medium","high","dead")
  for(i in 1:10){
    carl[i,1]<-length(which(avocado.groves[(size.of.mat*(i-1)*12+1):(size.of.mat*(i-1)*12+size.of.mat),3]=="low"))
    carl[i,2]<-length(which(avocado.groves[(size.of.mat*(i-1)*12+1):(size.of.mat*(i-1)*12+size.of.mat),3]=="medium"))
    carl[i,3]<-length(which(avocado.groves[(size.of.mat*(i-1)*12+1):(size.of.mat*(i-1)*12+size.of.mat),3]=="high"))
    carl[i,4]<-length(which(avocado.groves[(size.of.mat*(i-1)*12+1):(size.of.mat*(i-1)*12+size.of.mat),3]=="dead"))
  }
#View(carl)
  
#calculate average change across stubbornness
  
toby.mat<-matrix(0,nrow=2,ncol=size.of.mat)
for(i in 1:size.of.mat){
  toby<-filter(avocado.groves,avocado.groves$node==i)
  toby.count<-0
  for(j in 1:119){
    if(toby[j,3]!=toby[j+1,3] && toby[j+1,3]!="dead"){
        toby.count=toby.count+1
      }
    }
    toby.mat[1,i]<-avocado.groves[[i,6]]
    toby.mat[2,i]<-toby.count 
} #a place holder mat with stubbornness and # of changes
  
  ab.stb<-matrix(0,nrow=2,ncol=10)
  for(i in stubborness.lower.bound:stubborness.upper.bound){
    ab.stb[1,i]<-(i/10)
    ab.stb[2,i]<-sum(toby.mat[2,which(toby.mat[1,]==(i/10))])/length(which(toby.mat[1,]==(i/10)))
  }#this shows AVERAGE number of changes throughout a simulation for each stubbornness value
  #View(ab.stb)
  
  #calculate 95th and 5th percentile of finances
  
  money95.1<-order(avocado.groves[1:size.of.mat,5],decreasing=FALSE)
  money95.1<-as.matrix(avocado.groves[money95.1,5])
  n1<-round(0.95*length(money95.1))
  #sum(money95.1[1:n1])/n1
  n2<-round(0.05*length(money95.1))
  #sum(money95.1[1:n2])/n2
  money95.120<-order(avocado.groves[(size.of.mat*10*12+1):(size.of.mat*10*12+size.of.mat),5],decreasing=FALSE)
  money95.120<-as.matrix(avocado.groves[money95.120,5])
  n3<-round(0.95*length(money95.120))
  #sum(money95.120[1:n3])/n3
  n4<-round(0.05*length(money95.120))
  #sum(money95.120[1:n4])/n4
  
  #calculate 95th and 5th percentile of grove infections
  
  prop95.1<-avocado.groves[(size.of.mat*10*12+1):(size.of.mat*10*12+size.of.mat),12]/avocado.groves[1:size.of.mat,12]
  prop95.1<-as.matrix(prop95.1)
  for(i in 1:size.of.mat){
    if(prop95.1[i]<0){
      prop95.1[i]<-0
    }
  }
  p<-order(prop95.1,decreasing=FALSE)
  prop95.1<-prop95.1[p]
  prop95.1<-as.matrix(prop95.1)
  n5<-round(0.95*length(prop95.1))
  #sum(prop95.1[1:n5])/n5
  n6<-round(0.05*length(prop95.1))
  #sum(prop95.1[1:n6])/n6
  
  #calculate percent infected at end by stubborneess
  
  bob.mat<-matrix(0,nrow=2,ncol=10)
  for(i in 1:10){
    
    bob<-filter(avocado.groves[1:size.of.mat,],avocado.groves[1:size.of.mat,6]==(i/10))
    obo<-filter(avocado.groves[(size.of.mat*10*12+1):(size.of.mat*10*12+size.of.mat),],avocado.groves[(size.of.mat*10*12+1):(size.of.mat*10*12+size.of.mat),6]==(i/10))
    
    if(dim(bob)[1]==0){
      bob.mat[1,i]<-(i/10)
      bob.mat[2,i]<-0
    }
    else{
      for(k in 1:dim(obo)[1]){
        if(obo[k,12]==-1){
          obo[k,12]<-0
        }
      }
      
      bob.mat[1,i]<-(i/10)
      bob.mat[2,i]<-sum(obo[,12])/sum(bob[,12])
    }
  }
# SAVE LIST
ULTI.LIST <- list()
# (W=W)
ULTI.LIST <- list( 
    trees = jazz, # % final trees remaining / initial trees 
    strats.b = carl[1,1:3], # number of low,med,high strats at the beginning
    strats.e = carl[10,1:3], # number of low,med,high strats at the end
    x.stub = ab.stb[2,1:10], # number of average changes made per stubbornness  
    money951.1 = sum(money95.1[1:n1])/n1, #
    money952.1 = sum(money95.1[1:n2])/n2,
    money951.120 = sum(money95.120[1:n3])/n3,
    money952.120 = sum(money95.120[1:n4])/n4,
    prop95.1 = sum(prop95.1[1:n5])/n5,
    prop95.2 = sum(prop95.1[1:n6])/n6,
    bob = bob.mat[2,1:10]
  )
  print(ULTI.LIST)
}

###################################################################################################################

#initializing a for loop to repeat the simulation 400 times	

ULTIMATE.MAT<-matrix(0,nrow=10,ncol=33)
colnames(ULTIMATE.MAT)<-c("Percent Grove Remaining","# Inital Low","# Inital Med", "# Inital High", "# Final Low","# Final Med","# Final High", 
                          "0.1 A","0.2 A","0.3 A","0.4 A","0.5 A","0.6 A","0.7 A","0.8 A","0.9 A","1.0 A","95th Finances Inital",
                          "5th Finances  Inital","95th Finances Final","5th Finances Final",
                          "95th Disease","5th Disease", "0.1 S","0.2 S","0.3 S","0.4 S","0.5 S",
                          "0.6 S","0.7 S","0.8 S","0.9 S","1.0 S") #data being save from each simulation

###################################################################################################################

#Files and unchanged paramters

av.centers<- read_xlsx("/blue/garrett/betherton/LW2020/av.centers.xlsx")
#av.centers<- read_xlsx("av.centers.xlsx")
#av.centers<-av.centers[1:50,] #for just 50 groves 
distance<-read_xlsx("/blue/garrett/betherton/LW2020/Distance.xlsx")
#distance<-read_xlsx("Distance.xlsx")
distance<-as.data.frame(distance)
#distance<-distance[1:50,1:50] #for just 50 groves


size<-dim(av.centers)[1] #size of matrix
mean.income<-5122  #mean income for growers before costs are applied
sd.income<-750   #standard dev. for growers income before costs
pod.initial<-0.05 #probability of disease initially; at time step one
sms.weight<-0.4 #same management strategy weight
sd.low.mgmt.growth=24 #standard deviation of pathogen movement with low management
sd.medium.mgmt.growth=8 #standard deviation of pathogen movement with medium management
sd.high.mgmt.growth=4 #standard deviation of pathogen movement with high management
loss.from.yeild=51 #how much $ per tree lost per year
cost.per.tree.sick.medium=10 #the cost medium management pays per sick tree
cost.per.tree.sick.high=20  #the cost high management pays per sick tree
cost.per.tree.health.med=0.5  #the cost medium management pays per healthy tree
cost.per.tree.health.high=1  #the cost high management pays per healthy tree
pcos<-0.6 #60% chance of negligent growers getting penalized
stick.perc.increase<-10  #for sticks, what percent of annual income is removed from grower

##################################################################################################################

#Now we have the variables and objects which will change throughout the simulations:

##################################################################################################################
#Parameters
# 
args <- commandArgs(TRUE)
ARG <- as.numeric(strsplit(args[1], ",")[[1]])

#ARG = c(2.0, 1.5, 1, 10, 150000, 10)

print(ARG)

(beta.bp <- ARG[1]) #2.0  #beta value for biphysical network
(beta.sn <- ARG[2]) #1.5  #beta value for social network
(stubborness.lower.bound <- ARG[3])  #1 #the range of stubbornness can be between 1 and 10
(stubborness.upper.bound <- ARG[4]) #10 #this is the upper bound
(money.pot <- ARG[5])  #150000  #for carrots, the yearly pot of money
(stick.perc.increase <- ARG[6]) #10  #for sticks, what percent of annual income is removed from grower

######################################################################################################

#Setting up Data Matrix Used for Analysis
ULTIMATE.LIST <- list()

#assign initial capital, initial stubbornness and the timestep of 0:
# abm_function <- function(av.centers){
  avocado.groves <- av.centers %>% 
  mutate(capital = av.centers$acres * rnorm(1, mean = mean.income, sd = sd.income)) %>% 
  mutate(stubbornness = (sample(stubborness.lower.bound:0.1:stubborness.upper.bound,size,replace=TRUE)/10)) %>% 
  mutate(timestep = 0)

#make it so there are only three management strategies with "high", "medium" and "low" management:
for(i in 1:size){
  if(avocado.groves[i,6]=="scout & rogue"){
    avocado.groves[i,6]<-"high"
  }
  if(avocado.groves[i,6]=="fungicide"){
    avocado.groves[i,6]<-"high"
  } 
  if(avocado.groves[i,6]=="nothing"){
    avocado.groves[i,6]<-"low"
  }
  if(avocado.groves[i,6]=="stumping"){
    avocado.groves[i,6]<-"medium"
  }
} 

#filter out lat and longitude data:
avocado.groves <- avocado.groves %>% 
  select("timestep", "Node", "treat", "acres", "capital", "stubbornness") %>% 
  #create initial perceptions about growing strategies:
  mutate(Perc.low=0, Perc.medium= 0, Perc.high= 0)

for(i in 1:size){
  if(avocado.groves[i,3]=="low"){ #if you practice low management
    avocado.groves[i,7]<-avocado.groves[i,5] #your income reflects your beliefs
    avocado.groves[i,8]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4] #and all other strategies earn you less
    avocado.groves[i,9]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4] #this will also earn you less
  }
  if(avocado.groves[i,3]=="medium"){ #if you practice medium management
    avocado.groves[i,8]<-avocado.groves[i,5] #your income reflects your beliefs
    avocado.groves[i,7]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]
    avocado.groves[i,9]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]
  }
  if(avocado.groves[i,3]=="high"){ #if you practice high management
    avocado.groves[i,9]<-avocado.groves[i,5] #your income reflects your beliefs
    avocado.groves[i,7]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]
    avocado.groves[i,8]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]
  }
} 

#assign initial disease presence, and initial count for healthy/diseased trees within a grove:
avocado.groves<-mutate(avocado.groves,disease.presence=sample(c(0,1),size,replace=TRUE,prob=c((1-pod.initial),pod.initial)))%>%
  mutate(avocado.groves,diseased.trees=0)%>%
  mutate(healthy.trees=0) 

#there are 100 trees per grove:
for(i in 1:size){
  avocado.groves[i,12]<-round(avocado.groves[i,4]*100)
}

#the row for tracking compounding costs
avocado.groves<-mutate(avocado.groves,yearly.costs=0) 

#renaming column names:
colnames(avocado.groves)<-c("timestep","node","treatment","acres","capital","stubbornness",
                            "perc.Low","perc.Medium","perc.High","disease.presence","diseased.trees",
                            "healthy.trees","monthly.costs")
(avocado.groves)

#########################################################################################################

#This is setting up the social adjacency matrix

#a matrix of weights if two groves practice the same management strategy:
social.net<-matrix(0.1,nrow=size,ncol=size) 
for(i in 1:size){
  for(j in 1:size){
    if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="low" ){
      social.net[i,j]<-(sms.weight) 
    }
    if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="medium" ){
      social.net[i,j]<-(sms.weight)
    }
    if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="high" ){
      social.net[i,j]<-(sms.weight)
    }
  }
}

#inverse power law of distance between two groves:
distance.net<-matrix(0,nrow=size,ncol=size)
for(i in 1:size){
  for(j in 1:size){
    if(distance[i,j]==0){
      distance.net[i,j]<-0
    }
    else{
      distance.net[i,j]<-distance[i,j]^-beta.sn 
    }
  }
}

#element-wise multiplication:
social.net<-social.net*distance.net 

#rescale between 0 and 1:
min.sn<-min(social.net)
max.sn<-max(social.net)
rescaled.social.net<-matrix(0,ncol=size,nrow=size)
for(i in 1:size){
  for(j in 1:size){
    rescaled.social.net[i,j]<-((social.net[i,j]-min.sn)/(max.sn-min.sn))
  }
}

#a link based off probability of communicating from rescaled matrix:
comm.net<-matrix(0,ncol=size,nrow=size)
for(i in 1:size){
  for(j in 1:size){
    cn<-sample(c(0,1),1,prob=c(1-rescaled.social.net[i,j],rescaled.social.net[i,j]))
    comm.net[i,j]<-cn
  }
} 
	
for(i in 1:size){
  for(j in 1:size){
    if(comm.net[i,j]==1){
      comm.net[j,i]<-1
    }
  }
}

comm.net<-as.data.frame(comm.net)
colnames(comm.net)<-1:size
comm.net[1:5,1:5]

#########################################################################################################

#This is setting up the biophysical network

#gravity model matrix:
bio.net<-matrix(0,nrow=size,ncol=size)
for(i in 1:size){
  for(j in 1:size){
    bio.net[i,j]<-avocado.groves$acres[i]*avocado.groves$acres[j]*distance[i,j]^-beta.bp
  }
} 

#rescale between 0 and 1:
min<-min(bio.net) 
diag(bio.net)<-0
max<-max(bio.net)
rescaled.bio.net<-matrix(0,nrow=size,ncol=size)
for(i in 1:size){
  for(j in 1:size){
    rescaled.bio.net[i,j]<-((bio.net[i,j]-min)/(max-min))
  }
} 
diag(rescaled.bio.net)<-0
#View(rescaled.bio.net)

#read in two nodes, recieve a 0 or 1 based off the probability of movement between the two groves
bp.net<-function(node1,node2){
  b<-rescaled.bio.net[node1,node2]
  p<-sample(c(0,1),1,prob=c(1-b,b))
  return(p)
} 

#########################################################################################################
library(doParallel)

registerDoParallel(cores=12)  

ULT.MAT <- foreach(W=seq_along(1:100)) %dopar% 
  the.entire.process(size, 120) #this will simulate disease spread and 
                                   #growers decisions over 10 years (120 months)
str(ULT.MAT)

#########################################################################################################

#########################################################################################################

NAME <- paste(ARG[1], ARG[2], ARG[3], ARG[4], ARG[5], ARG[6], sep="_")  
save.image(paste0("/blue/garrett/betherton/LW2020/","ABMModel", NAME, ".RData"))

NAME <- paste(as.character(ARG[1]), 
	      as.character(ARG[2]), 
	      as.character(ARG[3]), 
	      as.character(ARG[4]), 
	      as.character(ARG[5]), 
	      as.character(ARG[6]), sep = "-")

# saving image 

save.image(paste0("../lw2021/","_ABMModel_", NAME, ".RData"))



