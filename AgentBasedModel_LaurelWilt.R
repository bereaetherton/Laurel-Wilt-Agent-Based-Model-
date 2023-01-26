#-------------------------------------------------------------------------------#

#Necessary Libs

library(readr)
library(readxl)
library(dplyr)
library(doParallel)

#-------------------------------------------------------------------------------#

#File Upload

av.centers<- read_xlsx("av.centers.xlsx")
distance<-read_xlsx("Distance.xlsx")
distance<-as.data.frame(distance)

#-------------------------------------------------------------------------------#

#Set Parameter Values

size<-dim(av.centers)[1] #size of matrix
mean.income<-5122  #mean income for growers before costs are applied
mean.proft<-1325 #how much growers actually make
sd.income<-750   #standard dev. for growers income before costs
pod.initial<-0.05 #probability of disease initially; at time step one
sms.weight<-0.4 #same management strategy weight
loss.from.yeild=51 #how much $ per tree lost per year
cost.per.tree.sick.medium=10 #the cost medium management pays per sick tree
cost.per.tree.sick.high=20  #the cost high management pays per sick tree
cost.per.tree.health.med=0.5  #the cost medium management pays per healthy tree
cost.per.tree.health.high=1  #the cost high management pays per healthy tree
pcos<-0.6 #60% chance of negligent growers getting penalized

#-------------------------------------------------------------------------------#

#Load Changing Parameters from ParameterTable.txt

args <- commandArgs(TRUE)
ARG <- as.numeric(strsplit(args[1], ",")[[1]])

(beta.bp <- ARG[1]) #1.5  #beta value for biophysical network
(beta.sn <- ARG[2]) #1.0  #beta value for social network
(stubborness.lower.bound <- ARG[3])  #1 #the range of stubbornness can be between 1 and 10
(money.pot <- ARG[4])  #150000  #for carrots, the yearly pot of money
(stick.perc.increase <- ARG[5]) #10  #for sticks, what percent of annual income is removed from grower

NAME <- paste(ARG[1], ARG[2], ARG[3], ARG[4], ARG[5], sep="_")  
print(NAME)

#-------------------------------------------------------------------------------#

#The Agent-Based Model

#for wrapping everything together (the entire model)
the.entire.process<-function(size.of.mat,t){   
  
  #Assign Capital and Stubbornness
  avocado.groves <- av.centers %>% 
    mutate(capital = av.centers$acres * 13.25*100) %>% 
    mutate(stubbornness = (stubborness.lower.bound/10)) %>% 
    mutate(timestep = 0)
  
  #Assign Mgmt Strategy (high,medium,low)
  for (i in 1:size){
    avocado.groves[[i,6]]<-sample(c("low","medium","high"),1,replace=TRUE,prob=c(0.33,0.33,0.33))
  }  
  
  #filter out lat and longitude data
  avocado.groves <- avocado.groves %>% 
    select("timestep", "Node", "treat", "acres", "capital", "stubbornness") %>% 
    #create initial perceptions about growing strategies:
    mutate(Perc.low=0, Perc.medium= 0, Perc.high= 0)
  
  #Assign Perceived Benefit
  for(i in 1:size){
    if(avocado.groves[i,3]=="low"){ #if you practice low management
      avocado.groves[i,7]<-avocado.groves[i,5] #your income reflects your beliefs
      avocado.groves[i,8]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
      avocado.groves[i,9]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
    }
    if(avocado.groves[i,3]=="medium"){ #if you practice medium management
      avocado.groves[i,8]<-avocado.groves[i,5] #your income reflects your beliefs
      avocado.groves[i,7]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
      avocado.groves[i,9]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
    }
    if(avocado.groves[i,3]=="high"){ #if you practice high management
      avocado.groves[i,9]<-avocado.groves[i,5] #your income reflects your beliefs
      avocado.groves[i,7]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
      avocado.groves[i,8]<-avocado.groves[i,5]-sample(1:sd.income,1)*avocado.groves[i,4]#all other strategies earn you  perceive less value from
    }
  } 
  
  #assign initial disease presence, and initial count for healthy/diseased trees within a grove:
  avocado.groves<-mutate(avocado.groves,disease.presence=sample(c(0,1),size,replace=TRUE,prob=c((1-pod.initial),pod.initial)))%>%
    mutate(avocado.groves,diseased.trees=0)%>% #no trees are diseased at time 0
    mutate(healthy.trees=0) 
  
  #there are 100 trees per grove:
  for(i in 1:size){
    avocado.groves[i,12]<-round(avocado.groves[i,4]*100)
  }
  
  #the column for tracking yearly compounding costs
  avocado.groves<-mutate(avocado.groves,yearly.costs=0) 
  
  #renaming column names
  colnames(avocado.groves)<-c("timestep","node","treatment","acres","capital","stubbornness",
                              "perc.Low","perc.Medium","perc.High","disease.presence","diseased.trees",
                              "healthy.trees","monthly.costs")
  #head(avocado.groves)
  
  #-----------------------------------------------------------------------------#
  
  #Multilayer Networks
  
  #a matrix of weights if two groves practice the same management strategy:
  social.net<-matrix(0.1,nrow=size,ncol=size) 
  for(i in 1:size){
    for(j in 1:size){
      if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="low"){
        social.net[i,j]<-(sms.weight) 
      }
      if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="medium"){
        social.net[i,j]<-(sms.weight)
      }
      if(avocado.groves[i,3]==avocado.groves[j,3]&avocado.groves[j,3]=="high"){
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
  min.sn<-min(social.net);max.sn<-max(social.net)
  rescaled.social.net<-matrix(0,ncol=size,nrow=size)
  for(i in 1:size){
    for(j in 1:size){
      rescaled.social.net[i,j]<-((social.net[i,j]-min.sn)/(max.sn-min.sn))
    }
  }
  
  #a social link based off probability of communicating from rescaled matrix:
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
  head(comm.net)
  
  #-----------------------------------------------------------------------------#
  
  #gravity model matrix
  
  bio.net<-matrix(0,nrow=size,ncol=size)
  for(i in 1:size){
    for(j in 1:size){
      bio.net[i,j]<-avocado.groves$acres[i]*avocado.groves$acres[j]*distance[i,j]^-beta.bp
    }
  } 
  
  #rescale between 0 and 1:
  min<-min(bio.net);diag(bio.net)<-0;max<-max(bio.net)
  rescaled.bio.net<-matrix(0,nrow=size,ncol=size)
  for(i in 1:size){
    for(j in 1:size){
      rescaled.bio.net[i,j]<-((bio.net[i,j]-min)/(max-min))
    }
  } 
  diag(rescaled.bio.net)<-0
  rescaled.bio.net[1:10,1:10]
  
  #read in two nodes, receive a 0 or 1 based off the probability of movement between the two groves
  bp.net<-function(node1,node2){
    b<-rescaled.bio.net[node1,node2]
    p<-sample(c(0,1),1,prob=c(1-b,b))
    return(p)
  } #this represents the stochasticity of pathogen movement
  
  #-----------------------------------------------------------------------------#
  
  
  #Perceieved Benefit 
  
  #i = mgmt level of grower j *as character string*
  #j = grower of focus
  #t = time by months; time starts at 0; one year is months 0-11
  #k = connected grower of interest
  
  #NOTE: the NODE is the growers number i.e. grower #10. the ROW is the matrix row associated with the 
  #      NODE at a specific time i.e. grower (NODE) #10 at time t=1 may be ROW 1142
  
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
  
  # counts connections of j with management i at time t
  # read in NODE of interest, with i as character string, and time of interest:
  mgmtconnect<-function(i,j,t){
    count<-0
    for (w in 1:size){
      count<-count+(mgmtlevel(i,w,t)*mgmtlinks(j,w)) 
    }
    return(count)
  }
  
  # returns perceived benefit grower j has of other farmers
  # k who practice management i:
  grovebenefit<-function(i,j,t){
    av<-0
    for(w in 1:size){
      av<-av+(avocado.groves[[w+size*t,5]]*mgmtlevel(i,w,t)*mgmtlinks(j,w)) 
    }
    gb<-av 
    if(is.finite(gb)==TRUE){
      return(gb)
    }
    else{
      return(0)
    }
  }
  
  # cumulative perceived benefit for management i or grower j at time t
  # C_ijt = stubbornness * C_ij (t-1) + (1-stubbornness) * [stubbornness * perception_of_management + (1-stubbornness) * grovebenefit]
  # the break down of this is in the document: 
  cumperbenefit<-function(i,j,t){
    if(avocado.groves[j+size*(t-1),3]=="low"){
      L<-avocado.groves[j+size*(t-1),5]
      M<-0
      H<-0
    }
    if(avocado.groves[j+size*(t-1),3]=="medium"){
      L<-0
      M<-avocado.groves[j+size*(t-1),5]
      H<-0
    }
    if(avocado.groves[j+size*(t-1),3]=="high"){
      L<-0
      M<-0
      H<-avocado.groves[j+size*(t-1),5]
    }
    if(avocado.groves[j+size*(t-1),3]=="dead"){
      L<-0
      M<-0
      H<-0
    }
    if(i=="low"){
      c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),7]+(1-avocado.groves[j,6])*(L+grovebenefit(i,j,(t-1)))/(mgmtconnect(i,j,t)+1)
    }
    if(i=="medium"){
      c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),8]+(1-avocado.groves[j,6])*(M+grovebenefit(i,j,(t-1)))/(mgmtconnect(i,j,t)+1)
    }
    if(i=="high"){
      c.ben<-avocado.groves[j,6]*avocado.groves[j+size*(t-1),9]+(1-avocado.groves[j,6])*(H+grovebenefit(i,j,(t-1)))/(mgmtconnect(i,j,t)+1)
    }
    return(c.ben)
  } 
  
  #---------------------------------------------------------------------------------------------#
  
  #Epidemic Simulations
  
  #calculating within-grove spread:
  how.much.disease.is.there.within.grove<-function(N){
    if(avocado.groves[N-size,10]==1){ #if disease is present at previous time step
      if(avocado.groves[N-size,3]=="low"){  #if the management strategy is low
        s<-sample(c(0,1),1,prob=c(0.05,0.95)) #5% chance of stopping disease
        if(s==1){ #if unsuccessful management
          pathogen<-sample(4:8,1) #new diseased trees increases by 4 to 8 trees
        }
        else{ #if successful management
          pathogen<-0 #there is no new disease
        }
      }
      if(avocado.groves[N-size,3]=="medium"){  #if the management strategy is medium
        s<-sample(c(0,1),1,prob=c(0.4,0.6)) #40% chance of stopping disease
        if(s==1){ #if unsuccessful management
          pathogen<-sample(2:4,1) #new diseased trees increases by 2 to 4
        }
        else{ #if successful management
          pathogen<-0 #there is no new disease
        }
      }
      if(avocado.groves[N-size,3]=="high"){   #if the management strategy is high
        s<-sample(c(0,1),1,prob = c(0.9,0.1)) #90% chance of stopping disease
        if(s==1){ #if unsuccessful management
          pathogen<-sample(1:2,1) #new diseased trees increases by 1 to 2 trees
        }
        else{ #if successful management
          pathogen<-0 #there is no new disease
        }
      }
      new.disease<-pathogen #new.disease=old disease + new growth from functions above
      return(new.disease)
    }
    else{ #if theres no disease at previous time step
      return(0) #there is no disease
    }
  } 
  
  # how many healthy trees are there in a grove:
  how.many.healthy.trees.are.there<-function(N,sck){
    if(avocado.groves[N-size,10]==1){ #if there is disease in the grove 
      health<-(avocado.groves[[N-size,12]]-sck) #healthy trees = 100 trees per acre - diseased trees(from function above)
      return(health)
    }
    else{ #if there isn't disease
      health<-round(avocado.groves[[N-size,4]]*100) #the healthy trees remain the same
      return(health)
    }
  } 
  
  # calculating monthly compounding costs:
  how.much.did.this.cost.monthly<-function(N){
    if(avocado.groves[N-size,3]=="low"){ #if you practice low management 
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
  
  # calculating the new yearly costs:
  how.much.did.this.cost.newyear<-function(N,t){
    if(avocado.groves[N-size,3]=="low"){ #if you practice no management
      money<-avocado.groves[[N-size,13]]+stick(N)
    } 
    else if(avocado.groves[N-size,3]=="medium"){ #if you practice medium management
      money<-(avocado.groves[[N-size,12]])*cost.per.tree.health.med+avocado.groves[[N-size,11]]*cost.per.tree.sick.medium+avocado.groves[[N-size,13]]+stick(N)
    } 
    else if(avocado.groves[N-size,3]=="high"){ #if you practice high management
      money<-(avocado.groves[[N-size,12]])*cost.per.tree.health.high+avocado.groves[[N-size,11]]*cost.per.tree.sick.high+avocado.groves[[N-size,13]]+stick(N)
    }
    else if(avocado.groves[N-size,3]=="dead"){ #if grove is dead
      money<-0
    } 
    return(money)
  }
  
  # determining if diseased has spread to a grove:
  is.there.disease.here<-function(N,t){
    if(avocado.groves[N-size,10]==1){ #if there's already disease here...
      return(1) #there's still disease here duh
    }
    else{
      for(i in 1:size){ #otherwise...
        if(avocado.groves[i+size*(t-1),10]==1 && bp.net(N-size*t,i)==1){ 
          return(1) #if there is disease at grove i (your neighbor), and the biophysical network returns 1, there will be movement
        }
      }
      return(0) #otherwise no disease spread
    }
  }
  
  #Carrots and Sticks
  stick<-function(N){
    if(avocado.groves[N-size,3]=="high" || avocado.groves[N-size,3]=="medium"){
      mny<-avocado.groves[N-size,4]*0.05*100*13.25
      return(mny)
    }
    else{
      s<-sample(c(0,1),1,prob=c(1-pcos,pcos))
      if(s==0){
        mny<-avocado.groves[N-size,4]*0.05*100*13.25
        return(mny)
      }
      else{
        if(avocado.groves[N-size,10]==1){
          mny<-mny<-avocado.groves[N-size,4]*100*13.25*(stick.perc.increase/100)
          return(mny)
        }
        else{
          return(0)
        }
      }
    }
  }
  
  carrot<-function(N){
    if(avocado.groves[N-size,3]=="low"){
      money<-how.much.did.this.cost.monthly(N) 
      return(money)
    }
    else{
      if(money.pot<=0){
        money<-how.much.did.this.cost.monthly(N) 
        return(money)
      }
      else if(avocado.groves[N-size,11]==0){ 
        money<-how.much.did.this.cost.monthly(N) 
        return(money) 
      }
      else{
        money.pot<<-money.pot-how.much.did.this.cost.monthly(N)*0.5 
        costs.covered<-how.much.did.this.cost.monthly(N)*0.5 
        return(costs.covered)
      }
    }
  }
  
  #for filling in data by the month
  data.fill.monthly<-function(N,t){
    
    sick<-how.much.disease.is.there.within.grove(N)
    
    cash<-carrot(N) 
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
    avocado.groves[N,11]<<-sick 
    avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N,sick) 
    avocado.groves[N,13]<<-cash 
    
    if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){ #if you run out of money or trees
      avocado.groves[N,3]<<-avocado.groves[N-size,3]
      avocado.groves[N,5]<<-0
      avocado.groves[N,10]<<--1
      avocado.groves[N,11]<<--1
      avocado.groves[N,12]<<--1
      avocado.groves[N,13]<<--1
    }
  }
  
  #for filling in data for the turn of the new year: 
  data.fill.time12<-function(N,t){
    
    perc.not<-cumperbenefit("low",N-size*t,t-1) 
    perc.stu<-cumperbenefit("medium",N-size*t,t-1) 
    perc.fun<-cumperbenefit("high",N-size*t,t-1) 
    
    if(perc.not>perc.fun && perc.not>perc.stu){ #will you decide to adopt low management?
      man<-"low"
    }
    else if(perc.stu>perc.fun && perc.stu>perc.not){ #will you decide to adopt medium management?
      man<-"medium"
    }
    else if(perc.fun>perc.not && perc.fun>perc.stu){ #will you decide to adopt high management?
      man<-"high"
    }
    else{
      man<-avocado.groves[N-size,3] #otherwise you'll use what you used the last year
    }
    
    cash<-how.much.did.this.cost.newyear(N,t) 
    
    avocado.groves[N,1]<<-t
    avocado.groves[N,2]<<-avocado.groves[N-size*t,2]
    avocado.groves[N,3]<<-man
    avocado.groves[N,4]<<-avocado.groves[N-size*t,4]
    avocado.groves[N,5]<<-avocado.groves[N-size,5]+avocado.groves[N-size,12]*13.25-cash 
    avocado.groves[N,6]<<-avocado.groves[N-size*t,6]
    avocado.groves[N,7]<<-perc.not
    avocado.groves[N,8]<<-perc.stu
    avocado.groves[N,9]<<-perc.fun
    avocado.groves[N,10]<<-is.there.disease.here(N,t) 
    avocado.groves[N,11]<<-0
    avocado.groves[N,12]<<-how.many.healthy.trees.are.there(N,0) 
    avocado.groves[N,13]<<-0
    
    if(avocado.groves[N-size,12]<=0 | avocado.groves[N-size,5]<=0){ #if you run out of capital or trees
      avocado.groves[N,3]<<-avocado.groves[N-size,3]
      avocado.groves[N,5]<<-0
      avocado.groves[N,10]<<--1
      avocado.groves[N,11]<<--1
      avocado.groves[N,12]<<--1
      avocado.groves[N,13]<<--1
    }
  }
  
  #-----------------------------------------------------------------------------#
  
  timestep<-1
  while(timestep<=t){
    print(timestep)
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
  
} #end of the entire agent-based model
#-------------------------------------------------------------------------------#

#for the HiPerGator: running simulations in parallel

registerDoParallel(cores=12)  

ULT.MAT <- foreach(W=seq_along(1:12)) %dopar% 
  the.entire.process(size, 120) #this will simulate disease spread for 120 months
str(ULT.MAT)

#---------------------------------------------------------------------------------#
