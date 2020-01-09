# exploring disturbances
# Scott's "hard coded a disturbance in the base code (see spadesCBMcore.R line 448)
# here trying to run it past that disturbance to see if it runs
# CBoisvenue June 6, 2018

# 1st I ran the code chunk in spades in spadesCBM.Rmd parent module
# here (below) I change the end time from 1999 (set on line 54 of spadesCBM.Rmd parent module)
# to 2052
end(spadesCBMout) <- 2052

# now I run a spades call again and create a new outsim
spadesCBMout1 <- spades(spadesCBMout,debug=TRUE)

##CONCLUSION: this disturbances is not being applied...need to check into this
# looks like the disturbance events where not made into spades events...might be easy to fix
# look into the annual event in the spadesCBMcore.r 'yearsEvents" not found...

#-------------------------------------------------------
# Disturbances need to be transparent
# writing a function to easily identify what dist happen in a sim
# Questions:
# bogus dist schedule for all pixels in 2001 - once simulations run until 2005 - did it happen?
# how can I check?
# where do dist get read in?
# what type of dist is it? tranfer values?
#
# Oct.11,2018

# Question 1-------------------------------------
# bogus dist schedule for all pixels in 2001 - once simulations run until 2005 - did it happen?
# how can I check?

# Answer: yes

  ## look at them like this:
  head(spadesCBMSim@.envir$disturbanceEvents)

  ## seems like this should show what is scheduled here...but really, it is a place holder
  ## the the disturbances in that specific sim year
  spadesCBMout@.envir$yearEvents

# this is what the current simulations spitout
outPixelGroupPools <- read.csv(file.path("outputs", "output1stand.csv"))
# this has all the pools (25+"Input" which seems to always be equal to 1) 
# plus a unique identifier for the line, for the pixelGroup (standindex)
# and has the age of the pixelGroup.
dim(outPixelGroupPools)
# there are 758 unique pixelGroups
length(unique(outPixelGroupPools$standindex))
# and this simulation was 16 years long with first end(sim) at 1999 and second end(sim) at 2005
## 12128/758 = 16
# all pixelGroup ages (outPixelGroupPools$age) reset in 2001
unique(outPixelGroupPools$age[8339:9096])
#[1] 1
#------------------------------------------------


# Question 2--------------------
# where do dist get read in?

# Answer: disturbance are user inputs so they are read-in in the spadesCBMinputs module.
# Specifically in the spadesCBMinputs.R file, right now in lines 209 and 210
sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2001,sim$nStands),rep(214,sim$nStands))
colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")

#-------------------------------


# Questions 3--------------------------
# what type of dist is it? tranfer values?

## IMPORTANT NOTE: 
# disturbances matrice go from a vector of 21 source pools, numbered 1 to 21
# to a vector of 25 sink pools numbered 1 to 26, skipping 25. Line 25 would be NO2.
# cbm_default tables do NOT defined pool names associated with pool numbers
# pooldef is created in spadesCBMdefault module and is a character vector of length 26
# BUT no numbers are really associated with these.
# I went back to the access-based version of CBM-CFS3 and pulled out the 
# tblSinkName and tblSourceName for CBM-CFS3
# the links are logical and I wrote them explicitely here:
# GitHub\spadesCBM\data\cbm_defaults\CBM_Source_Sink_Pools.xlsx
# source
unique(spadesCBMddist@.envir$cbmData@disturbanceMatrixValues[,2])
#[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
# sink
unique(spadesCBMddist@.envir$cbmData@disturbanceMatrixValues[,3])
#[1] 16 18 22 23 24 11 19 13 14 12 20 21 15 17 26  1  2  3  4  5  6  7  8  9 10
# creating a table of pool names and numbers used in the disturbance matrices

# for function if length(spadesCBMddist@.envir$pooldef)= 26{do this} if not write an error

poolNames <- as.data.frame(cbind(spadesCBMddist@.envir$pooldef[-1],c(1:24,26)))
names(poolNames) <- c("pool","dmPoolId")


# Answer: need a function for easy retreival of this information
# the DisturbanceMatrixID is specified when the events are read-in (see Answer to Question 2)

## Getting the DisturbanceMatrixID
matNum <- unique(spadesCBMddist@.envir$disturbanceEvents[,3])
# in this case there is only one dist - # 214
# the id (as a character) is in the 1st column of this matrix, the name in the second and the word
# description in the third
head(spadesCBMddist@.envir$cbmData@disturbanceMatrix)
# the DisturbanceMatrixID value corresponds to the line number, example DMID 214 is line 214 of 
# the cbmData@disturbanceMatrix table
# so to get a word description of what 214 means

## getting the description of the disturbances
spadesCBMddist@.envir$cbmData@disturbanceMatrix[matnum,3]
# description 
# "Salvage uprooting and burn for Boreal Plains"

# to get the source pool and the sink pool with the proportions transfered, 
# we can use this table
head(spadesCBMddist@.envir$cbmData@disturbanceMatrixValues)
# this is how to extra one disturbance matrix
mat214 <- spadesCBMddist@.envir$cbmData@disturbanceMatrixValues[which(spadesCBMddist@.envir$cbmData@disturbanceMatrixValues[,1]==matNum),]

# each source pool's proportions shuold add to 1
# check

mat214_1 <-sum(mat214[which(mat214[,2]==1),4])
mat <- as.data.frame(mat214)
# it would be good to add the names
names(poolNames) <- c("sinkName","sink_pool_id")
sinkNames <- merge.data.frame(poolNames,mat)

names(poolNames) <- c("sourceName","source_pool_id")
sourceNames <- merge.data.frame(poolNames,sinkNames)
clearDist <- sourceNames[,c(5,1:4,6)]


## NEXT:
# creat a list of data.frames where each data.frame is the explicit (as sourceNames)
# disturbance matrix
simDist <- function(sim){
  # put names to the pools
  poolNames <- as.data.frame(cbind(sim@.envir$pooldef[-1],c(1:24,26)))
  names(poolNames) <- c("pool","dmPoolId")
  
  # Getting the number of DisturbanceMatrixID
  matNum <- unique(sim@.envir$disturbanceEvents[,3])
  # matNum will be the lenght of the list of data.frames
  clearDists <- vector("list", length=length(matNum))
  
  # for each matNum, create a data.frame that explains the pool transfers
  for(i in 1:length(matNum)){
    # get the lines specific to the distMatrix in question
    matD <- as.data.frame(sim@.envir$cbmData@disturbanceMatrixValues[which(sim@.envir$cbmData@disturbanceMatrixValues[,1]==matNum[i]),])
    names(poolNames) <- c("sinkName","sink_pool_id")
    sinkNames <- merge.data.frame(poolNames,matD)
    
    names(poolNames) <- c("sourceName","source_pool_id")
    sourceNames <- merge.data.frame(poolNames,sinkNames)
    clearDists[[i]] <- sourceNames[,c(5,1:4,6)]
  }
  # each data.frame gets a descriptive name
  names(clearDists) <- sim@.envir$cbmData@disturbanceMatrix[matNum,3]
  # description 
  # "Salvage uprooting and burn for Boreal Plains"
  return(clearDists)
}

# What disturbances were used for the Boisvenue et al 2016 SK-Recliner runs?
# CLASSIFIED	DisturbanceType
# Fire	Wildfire
# Harvesting	Clearcut harvesting with salvage
# Road	Deforestation â€” Transportation â€” Salvage, uprooting and burn
# Lcondition	Generic 20% mortality
# Unclass	Generic 20% mortality










