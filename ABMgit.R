
library(readr)
library(readxl)
library(dplyr)

####################################################################################################

#Files

av.centers<- read_xlsx("av.centers.xlsx")
av.centers<-av.centers[1:25,] #for just 25 groves 
distance<-read_xlsx("Distance.xlsx")
distance<-as.data.frame(distance)
distance<-distance[1:25,1:25] #for just 25 groves

####################################################################################################

#Parameters

size<-dim(av.centers)[1] #size of matrix
beta.bp<-2.0  #beta value for biphysical network
beta.sn<-1.0  #beta value for social network
mean.income<-5122  #mean income for growers before costs are applied
sd.income<-750   #standard dev. for growers income before costs
stubborness.lower.bound<-1 #the range of stubbornness can be between 1 and 10
stubborness.upper.bound<-10 #this is the upper bound
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
money.pot<-150000  #for carrots, the yearly pot of money
stick.perc.increase<-10  #for sticks, what percent of annual income is removed from grower

######################################################################################################

#Setting up Data Matrix Used for Analysis

#my goal later is to redo this section to make it so that a user can read in their own data

#for now this bit is pretty sloppy 

#assign initial capital, initial stubbornness and the timestep of 0:
avocado.groves<-mutate(av.centers,capital=av.centers$acres*rnorm(1,mean=mean.income,sd=sd.income))
avocado.groves<-mutate(avocado.groves,stubbornness=(sample(stubborness.lower.bound:0.1:stubborness.upper.bound,size,replace=TRUE)/10))
avocado.groves<-mutate(avocado.groves,timestep=0) 

#make it so there are only three management strategies with high,medium and low management:
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
avocado.groves<-avocado.groves[,c(9,1,6,5,7,8)]


#create initial perceptions about growing strategies:
avocado.groves<-mutate(avocado.groves,Perc.low=0,Perc.medium=0,Perc.high=0)
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
#View(avocado.groves)

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
comm.net<-as.data.frame(comm.net)
colnames(comm.net)<-1:size
#View(comm.net)

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


#These are the functions used to calculate perceived benefit after t=0

#i mgmt level of grower j *as character string*
#j grower of focus
#t time by months; time starts at 0; one year is months 0-11
#k connected grower of interest

#NOTE: the NODE is the growers number i.e. grower #10. the ROW is the matrix row associated with the 
#      NODE at a specific time i.e. grower (NODE) #10 at time t=1 may be ROW 1142



#1 if grower j and k and are linked in comm.net
#read in NODE number of interest:
mgmtlinks<-function(j,k){
  if(comm.net[j,k]==1){
    return(1)
  }
  else{
    return(0)
  }
} 

#1 if grower k practices management i at time t 
#read in NODE number of interest:
mgmtlevel<-function(i,k,t){
  if(avocado.groves[k+size*t,3]==i){
    return(1)
  }
  else{
    return(0)
  }
} 

#counts connections of j with management i at time t
#read in NODE of interest, with i as character string, and time of interest:
mgmtconnect<-function(i,j,t){
  count<-0
  for (w in 1:size){
    count<-count+(mgmtlevel(i,w,t)*mgmtlinks(j,w))
  }
  return(count)
}

#returns perceived benefit grower j has of other farmers
#k who practice management i:
grovebenefit<-function(i,j,t){
  av<-0
  for(w in 1:size){
    av<-av+(avocado.groves[[w+size*t,5]]*mgmtlevel(i,w,t)*mgmtlinks(j,w))
  }
  gb<-av/(mgmtconnect(i,j,t))
  if(is.finite(gb)==TRUE){
    return(gb)
  }
  else{
    return(0)
  }
}

#cumulative percieved benefit for management i or grower j at time t
#C_ijt=stubbornness*C_ij(t-1)+(1-stubbornness)*[stubbornness*perception_of_management+(1-stubbornness)*grovebenefit]
#the break down of this is in the document: 
cumperbenefit<-function(i,j,t){
  if(i=="low"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*t,7]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*t,7]+(1-avocado.groves[j,6])*grovebenefit(i,j,t))
  }
  if(i=="medium"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*t,8]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*t,8]+(1-avocado.groves[j,6])*grovebenefit(i,j,t))
  }
  if(i=="high"){
    c.ben<-avocado.groves[j,6]*avocado.groves[j+size*t,9]+(1-avocado.groves[j,6])*(avocado.groves[j,6]*avocado.groves[j+size*t,9]+(1-avocado.groves[j,6])*grovebenefit(i,j,t))
  }
  return(c.ben)
}

#########################################################################################################

#These are the functions used for moving through time

#a function to calculate within-grove spread:
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

#how many healthy trees are there in a grove:
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

#a function to calculate monthly compounding costs:
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

#a function to calculate the new yearly costs:
how.much.did.this.cost.newyear<-function(N){
  if(avocado.groves[N-size,3]=="low"){ #if you practice no management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+avocado.groves[[N-size,13]]
  } #costs(t+1)=sick.trees(t)*yearly.loss.from.yield + costs(t)
  else if(avocado.groves[N-size,3]=="medium"){ #if you practice medium management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+(avocado.groves[[N-size,12]])*cost.per.tree.health.med+avocado.groves[[N-size,11]]*cost.per.tree.sick.medium+avocado.groves[[N-size,13]]
  }  #costs(t+1)=healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t) + sick.trees(t)*yearly.loss.from.yield
  else if(avocado.groves[N-size,3]=="high"){ #if you practice high management
    money<-avocado.groves[[N-size,11]]*loss.from.yeild+(avocado.groves[[N-size,12]])*cost.per.tree.health.high+avocado.groves[[N-size,11]]*cost.per.tree.sick.high+avocado.groves[[N-size,13]]
  } #healthy.trees(t)*cost.per.healthy.tree+sick.trees(t)*cost.per.sick.tree+costs(t) + sick.trees(t)*yearly.loss.from.yield
  else if(avocado.groves[N-size,3]=="dead"){ #if grove is ded
    money<-0
  } #you make no money
  return(money)
}

#a function to determine if diseased has spread to a grove:
is.there.disease.here<-function(N,t){
  if(avocado.groves[N-size,10]==1){ #if there's already disease here...
    return(1) #there's still disease here
  }
  else{
    for(i in 1:size){ #otherwise...
      if(avocado.groves[i+size*(t-1),10]==1 && bp.net(N-size*t,i)==1){ 
        return(1) #if there is disease at grove i, and the biophysical network returns i, there will be movement
      }
    }
    return(0) #otherwise no disease spread
  }
}

#a function for filling in data by the month
data.fill.monthly<-function(N,t){
  
  cash<-how.much.did.this.cost.monthly(N)
  
  avocado.groves[N,1]<<-t
  avocado.groves[N,2]<<-avocado.groves[N-size,2]
  avocado.groves[N,3]<<-avocado.groves[N-size,3]
  avocado.groves[N,4]<<-avocado.groves[N-size,4]
  avocado.groves[N,5]<<-avocado.groves[N-size,5]
  avocado.groves[N,6]<<-avocado.groves[N-size,6]
  avocado.groves[N,7]<<-avocado.groves[N-size,7]
  avocado.groves[N,8]<<-avocado.groves[N-size,8]
  avocado.groves[N,9]<<-avocado.groves[N-size,9]
  avocado.groves[N,10]<<-is.there.disease.here(N,t)
  avocado.groves[N,11]<<-how.much.disease.is.there.within.grove(N)
  avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N)
  avocado.groves[N,13]<<-cash
  
  if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){ #if no more trees or money
    avocado.groves[N,3]<<-"dead"
    avocado.groves[N,5]<<--1
    avocado.groves[N,10]<<--1
    avocado.groves[N,11]<<--1
    avocado.groves[N,12]<<--1
    avocado.groves[N,13]<<--1
  }
}

#a function for filling in data for the turn of the new year: 
data.fill.time12<-function(N,t){
  
  #new percieved benefits from functions above:
  perc.not<-cumperbenefit("low",N-size*t,t-1) 
  perc.stu<-cumperbenefit("medium",N-size*t,t-1)
  perc.fun<-cumperbenefit("high",N-size*t,t-1)
  
  #if one management has a higher benefit, grower will change strategy
  if(perc.not>perc.fun && perc.not>perc.stu){
    man<-"low"
  }
  if(perc.stu>perc.fun && perc.stu>perc.not){
    man<-"medium"
  }
  if(perc.fun>perc.not && perc.fun>perc.stu){
    man<-"high"
  }
  
  cash<-how.much.did.this.cost.newyear(N)
  
  avocado.groves[N,1]<<-t
  avocado.groves[N,2]<<-avocado.groves[N-size*t,2]
  avocado.groves[N,3]<<-man #new management straetgy
  avocado.groves[N,4]<<-avocado.groves[N-size*t,4]
  avocado.groves[N,5]<<-rnorm(1,mean=mean.income,sd=sd.income)*avocado.groves[N-size,4]-cash #new income - costs from last year
  avocado.groves[N,6]<<-avocado.groves[N-size*t,6]
  avocado.groves[N,7]<<-perc.not #new perception
  avocado.groves[N,8]<<-perc.stu #new perception
  avocado.groves[N,9]<<-perc.fun #new perception
  avocado.groves[N,10]<<-is.there.disease.here(N,t)
  avocado.groves[N,11]<<-0
  avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N)
  avocado.groves[N,13]<<-0 #new costs per year
  
  if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){ #if no more trees or no money
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

stick<-function(N,t){
  s<-sample(c(0,1),1,prob=c(0.6,0.4))
  if(s==0){
    return(0)
  }
  else{
    mny<-avocado.groves[[N-size*t,4]]*(stick.perc.increase)
    return(mny)
  }
}

carrot<-function(N){
  if(money.pot<=0){
    return(0)
  }
  else{
    money.pot<<-money.pot-avocado.groves[[N,13]]*0.5
    costs.covered<-avocado.groves[[N,13]]*0.5
    return(costs.covered)
  }
}

####################################################################################################################

#Putting everything together

#a function for wrapping everything together
the.entire.process<-function(size.of.mat,t){   #read in the size of the matrix and the number of MONTHS you'd like
  timestep<-1
  while(timestep<=t){
    if(timestep%%12!=0){
      for(N in 1:size.of.mat){
        data.fill.monthly(N+size.of.mat*timestep,timestep)
      }
    }
    if(timestep%%12==0){
      for(N in 1:size.of.mat){
        data.fill.time12(N+size.of.mat*timestep,timestep)
      }
    }
    timestep=timestep+1
  }
}

the.entire.process(size,120) #this will simulate disease spread and growers decisions over 10 years (120 months)

###################################################################################################################

