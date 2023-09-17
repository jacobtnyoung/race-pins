#Started on 9/21/18
#Corey Whichard
#PINS W1 Race Paper Analyse

# =============================================================================== #
# Call libraries.
library(sna)
library(network)
library(statnet)
library(ergm)
library(parallel)
library('readxl')
library("latticeExtra")
library("btergm")
library('gplots')
library(ggplot2)
library("ape")

# Clear workspace.
rm(list=ls())

#Corey's personal computer directory information:
setwd("C:\\Users\\CW883458\\Desktop\\Research\\Race Paper\\PINS Race\\R stuff")
#load("PINS_Wave_1_POWER_NETWORKS_FOR_ERGMS.RData") # see NOT RUN below for how this gets created.
load("PINS_Wave_1_NETWORKS_RACEATTITUDES_ANALYSIS.RData")


#Reading in additional racial attitude variables:
merge_vars <- read_excel("Merge Variables.xlsx")

#Recoding the comfort measure into a categorical variable
merge_vars$comfort.x <- merge_vars$comfort
merge_vars$comfort.x [(merge_vars$comfort.x)=="0 Less"]=0
merge_vars$comfort.x [(merge_vars$comfort.x)=="1 Same"]=1
merge_vars$comfort.x [(merge_vars$comfort.x)=="2 More"]=2
merge_vars$comfort.x[is.na(merge_vars$comfort.x)]=1
merge_vars$comfort.x <- as.numeric(merge_vars$comfort.x)
merge_varsx <- merge_vars[,-3] #Creating a new data frame object, deleting out the old "comfort level" vector


#Recoding missing values
merge_varsx$SameRaceRoom[is.na(merge_vars$SameRaceRoom)]=0


#Checking node order to make sure the attributes can be merged in properly:
merge_varsx$ID==rownames(as.sociomatrix(get.along.norank.excluded.net)) #Perfect. I can now load attributes into the network object.
set.vertex.attribute(get.along.norank.excluded.net, "sameraceroom", merge_varsx$SameRaceRoom) #This adds the same-race roommate variable to the network object.
set.vertex.attribute(get.along.norank.excluded.net, "pporf", merge_varsx$prefrndrace)
set.vertex.attribute(get.along.norank.excluded.net, "race_comfort", merge_varsx$comfort.x)

set.vertex.attribute(get.along.ex.race.homophily.net, "sameraceroom", merge_varsx$SameRaceRoom) #This adds the same-race roommate variable to the network object.
set.vertex.attribute(get.along.ex.race.homophily.net, "pporf", merge_varsx$prefrndrace)
set.vertex.attribute(get.along.ex.race.homophily.net, "race_comfort", merge_varsx$comfort.x)

set.vertex.attribute(get.along.ex.race.heterophily.net, "sameraceroom", merge_varsx$SameRaceRoom) #This adds the same-race roommate variable to the network object.
set.vertex.attribute(get.along.ex.race.heterophily.net, "pporf", merge_varsx$prefrndrace)
set.vertex.attribute(get.along.ex.race.heterophily.net, "race_comfort", merge_varsx$comfort.x)


#The network objects:
get.along.norank.excluded.net
get.along.ex.race.homophily.net
get.along.ex.race.heterophily.net

#Renaming networks for ease
full <- get.along.norank.excluded.net
hom <- get.along.ex.race.homophily.net
het <- get.along.ex.race.heterophily.net

#RECODING "SOCIAL DISTANCE" VARIABLE (rescaling it so that 0 is lowest value, 4 is highest)
get.along.norank.excluded.net %v% "Soc.Dist"
table(get.along.norank.excluded.net %v% "Soc.Dist")
mean(get.along.norank.excluded.net %v% "Soc.Dist", na.rm=T)
r.soc.dist <- get.along.norank.excluded.net %v% "Soc.Dist"
r.soc.dist <- r.soc.dist-1
mean(r.soc.dist, na.rm=T)
#Setting missing value to the sample mean
r.soc.dist[is.na(r.soc.dist)] = 0.722
#Adding recoded social distance measure to the networks:
set.vertex.attribute (full,"Soc.Dist.R",r.soc.dist)
set.vertex.attribute (hom,"Soc.Dist.R",r.soc.dist)
set.vertex.attribute (het,"Soc.Dist.R",r.soc.dist)

#RECODING DAYS ON UNIT VARIABLE
dou <- full %v% "DOU"
dou <- dou/100
#Adding recoded days on unit measure to the networks:
set.vertex.attribute (full,"DOU",dou)
set.vertex.attribute (hom,"DOU",dou)
set.vertex.attribute (het,"DOU",dou)


#CONSTRUCTING DATA-FRAME FOR ANALYZING SURVEY DATA
#The following section of code involves extracting node attribute data from the network objects and storing
#it in a data frame for various analyses.

id <- get.along.norank.excluded.net %v% "id"

get.along.norank.excluded.net

rrespct <- get.along.norank.excluded.net %v% "Race.Important.Respect"
rsoc <- get.along.norank.excluded.net %v% "Race.Important"
race <- get.along.norank.excluded.net %v% "Race"
table(race)

white <- race
black <- race
hisp <- race

white[(white)>1] = 0
table(white)

black[(black)<2]=0
black[(black)>2]=0
black[(black)>0]=1
table(black)

hisp[(hisp)<3]=0
hisp[(hisp)>3]=0
hisp[(hisp)>0]=1
table(hisp)

rcomfort <- get.along.norank.excluded.net %v% "race_comfort"
less_comfort <- rcomfort
less_comfort[(less_comfort)>0]=2
less_comfort[(less_comfort)<1]=1
less_comfort[(less_comfort)>1]=0
table(less_comfort)

same_comfort <- rcomfort
same_comfort[(same_comfort)>1]=0
same_comfort[(same_comfort)<1]=0
table(same_comfort)

more_comfort <- rcomfort
more_comfort[(more_comfort)<2]=0
more_comfort[(more_comfort)>0]=1
table(more_comfort)


sameraceroom <- get.along.norank.excluded.net %v% "sameraceroom"
pporf <- get.along.norank.excluded.net %v% "pporf"

religious <- het %v% "Religious.Attendance.Now"
table(religious)

soc.bel <- het %v% "Soc.Belng"
table(soc.bel)

grade <- het %v% "Grade"
table(grade)

dep <- het %v% "CESD.Depression"
table(dep)

daysunit <- het %v% "DOU"
table(daysunit)

time.in <- het %v% "Time.In"
table(time.in)

rel <- het %v% "Religion"
table(rel)

rel.id <- data.frame(id, rel)

#Generating a data-frame object containing racial attitudes variables and network position variables
racedat <- data.frame(r.soc.dist, rrespct, rsoc, less_comfort, same_comfort, more_comfort, sameraceroom, pporf, white, black, hisp)




#Creating data frame for matrix construction

#Relevant variables:
dist <- racedat$r.soc.dist
id <- rel.id$id
time_unit <- daysunit

#Changing american indian to white
table(race)
race[(race)==4]=1
table(race)

#Adding revised variables to the FULL network
set.vertex.attribute (full,"Race",race)
set.vertex.attribute (full,"rrespct",rrespct)
set.vertex.attribute (full,"rsoc",rsoc)





#REVIEWING ATTRIBUTE MEASURES FOR "FULL" GET ALONG NETWORK 

#MOST ANALYTICALLY RELEVANT VARIABLES:
#Social distance 
table(full %v% "Soc.Dist.R")

#Race (1 = white, 2 = black, 3 = Hispanic)
table(full %v% "Race")

#Change in comfort around people of other races (0 = less comfortable since incarceration, 1 = same, 2 = more comfortable)
table(full %v% "race_comfort")

#Living with cellmate/bunkmate of the same race (0 = no, 1 = yes)
table(full %v% "sameraceroom")

#Before incarceration, reported having at least 1 interracial friendship (0 = no, 1 = yes)
table(full %v% "pporf")

#Level of agreement with statement: "A prisoner's race is more important than anything else in determining receipt of respect"
table(full %v% "rrespct")

#Level of agreement with statement: "A prisoner's race is most important thing for determining who hangs together in prison"
table(full %v% "rsoc")


#CONTROL VARIABLES:
#Age at time of survey
table(full %v% "Age")

#Total years incarcerated, lifetime
table(full %v% "Time.In")

#Social belongingness on housing unit
table(full %v% "Soc.Belng")

#Total days spent living on the housing unit, divided by 100
table(full %v% "DOU")

#Religious identification, recorded at prison intake
table(full %v% "Religion")

#Hometown, reported at time of survey
table(full %v% "City")





#CONSTRUCTING DYADIC TERMS FOR EXAMINING INTERACTIONS (DOES SOCIAL DISTANCE MODERATE RACIAL HOMOPHILY?)

data <- data.frame(id, dist, race, time_unit, time.in)

#STEPS:
#1. Create ego/alter race matrices
#2. Create ego/alter social distance matrices
#3. Create racial homophily matrix
#4. Create ego social distance x racial homophily interaction term matrix
#5. Create alter social distance x racial homophily interaction term matrix

#Step 1: Ego/Alter Race
ego.race <- matrix(data$race,nrow = 133,ncol = 133, byrow=F) 
rownames(ego.race) <- id 
colnames(ego.race) <- id 
ego.race

alter.race <- matrix(data$race,nrow = 133,ncol = 133, byrow=T) 
rownames(alter.race) <- id 
colnames(alter.race) <- id 
alter.race

#STEP 2: Sender/Receiver Social Distance
ego.sd <- matrix(data$dist,nrow = 133,ncol = 133, byrow=F) 
rownames(ego.sd) <- id 
colnames(ego.sd) <- id 
ego.sd

alter.sd <- matrix(data$dist,nrow = 133,ncol = 133, byrow=T) 
rownames(alter.sd) <- id 
colnames(alter.sd) <- id 
alter.sd

#STEP 3: Racial homophily matrix
race.hom <-(ego.race==alter.race)
race.hom <- race.hom*1 
race.hom #Matrix where 1 = dyad ij is same race, 0 otherwise

#STEP 4: Ego social distance by racial homophily - interaction term matrix
egodistance.racehom <- ego.sd * race.hom

#STEP 5: Alter social distance by racial homophily - interaction term matrix
alterdistance.racehom  <- alter.sd * race.hom


indegree <- degree(full, gmode = "digraph", cmode = "indegree")




#CONSTRUCTING DYADIC TERMS FOR EXAMINING INTERACTIONS (DOES "TIME ON UNIT" MODERATE RACIAL HOMOPHILY?)

#STEP 1: Racial homophily matrix
#Note: This was created in block of code above
race.hom #Matrix where 1 = dyad ij is same race, 0 otherwise

#STEP 2: Sender/Receiver "Time on Unit" matrix
ego.time <- matrix(data$time_unit,nrow = 133,ncol = 133, byrow=F) 
rownames(ego.time) <- id 
colnames(ego.time) <- id 
ego.time

alter.time <- matrix(data$time_unit,nrow = 133,ncol = 133, byrow=T) 
rownames(alter.time) <- id 
colnames(alter.time) <- id 
alter.time


#STEP 3: Ego "time on unit" by racial homophily - interaction term matrix
egotime.racehom <- ego.time * race.hom

#STEP 4: Alter "time on unit" by racial homophily - interaction term matrix
altertime.racehom  <- alter.time * race.hom






#CONSTRUCTING DYADIC TERMS FOR EXAMINING INTERACTIONS (DOES "TIME In PRISON" MODERATE RACIAL HOMOPHILY?)

#STEP 1: Racial homophily matrix
#Note: This was created in block of code above
race.hom #Matrix where 1 = dyad ij is same race, 0 otherwise

#STEP 2: Sender/Receiver "Time In Prison" matrix
ego.prisontime <- matrix(data$time.in,nrow = 133,ncol = 133, byrow=F) 
rownames(ego.prisontime) <- id 
colnames(ego.prisontime) <- id 
ego.prisontime

alter.prisontime <- matrix(data$time.in,nrow = 133,ncol = 133, byrow=T) 
rownames(alter.prisontime) <- id 
colnames(alter.prisontime) <- id 
alter.prisontime


#STEP 3: Ego "time in prison" by racial homophily - interaction term matrix
egoprisontime.racehom <- ego.prisontime * race.hom

#STEP 4: Alter "time in prison" by racial homophily - interaction term matrix
alterprisontime.racehom  <- alter.prisontime * race.hom

egoprisontime.racehom
alterprisontime.racehom 






#FOR DESCRIPTIVE STATISTICS:

#Statistics to Calculate:
#Percent of sample (categorical)
#Mean and Standard Deviation (Continuous or interval)
#Connection with Indegree - Mean per categorical measure, correlation per continuous measure
#Homophily - Odds Ratio for categorical, Moran's I for continuos

#Measures:
#Race - White, Black, Hispanic
#Social Distance 
#Comfort Level
#Same-Race Cellmate
#Pre-Prison Interracial Friendship
#Age
#Prison Tenure
#Social Belongingness
#Days on Unit
#Religion
#City

#Storing all in one data-frame:
#Social distance 
soc.dist <- (full %v% "Soc.Dist.R")

#Race (1 = white, 2 = black, 3 = Hispanic)
race.cat <- (full %v% "Race")

#Change in comfort around people of other races (0 = less comfortable since incarceration, 1 = same, 2 = more comfortable)
comfort.change <- (full %v% "race_comfort")

#Living with cellmate/bunkmate of the same race (0 = no, 1 = yes)
samerace.cell <- (full %v% "sameraceroom")

#Before incarceration, reported having at least 1 interracial friendship (0 = no, 1 = yes)
pporf <- (full %v% "pporf")

#Age at time of survey
ageyears <- (full %v% "Age")

#Total years incarcerated, lifetime
yearsinc <- (full %v% "Time.In")

#Social belongingness on housing unit
socialbel <- (full %v% "Soc.Belng")

#Total days spent living on the housing unit, divided by 100
daysonunit <- (full %v% "DOU")

#Religious identification, recorded at prison intake
religion <- (full %v% "Religion")

#Hometown, reported at time of survey
hometown <- (full %v% "City")

descriptives <- data.frame(indegree, soc.dist, race.cat, comfort.change, samerace.cell, pporf, ageyears, yearsinc, socialbel, daysonunit, religion, hometown)


#Race - Percentages, Mean indegree per group, homophily odds ratio
table(descriptives$race.cat)
44+69+20
44/133
69/133
20/133

aggregate(descriptives$indegree, by=list(descriptives$race.cat), FUN=mean)

des1 <- ergm(full ~ nodematch("Race",diff=F) + edges)
summary(des1)
#Nodematch coefficient = 1.177
exp(1.177)
#Odds ratio = 3.25***

#White inmates = 44, 33.1% of sample; 4.23 mean
#Black inmates = 69, 51.9% of sample; 3.71 mean
#Hispanic inmates = 20, 15% of sample; 4.05 mean


#Social Distance
min(descriptives$soc.dist)
max(descriptives$soc.dist)
mean(descriptives$soc.dist)
sd(descriptives$soc.dist)
cor.test(descriptives$soc.dist, descriptives$indegree)

getalong.mat <- as.matrix(full)
Moran.I(descriptives$soc.dist, getalong.mat)


#Change in Comfort Around People of Other Races Since Incarceration
table(descriptives$comfort.change)
#Less comfortable = 16
#Same comfort level = 91
#More comfortable = 26
16 + 91 + 26
16/133
91/133
26/133
aggregate(descriptives$indegree, by=list(descriptives$comfort.change), FUN=mean)
des2 <- ergm(full ~ nodematch("race_comfort",diff=F) + edges)
summary(des2)
#Nodematch coefficient = 0.12
exp(0.11479)
#Odds ratio = 1.12


#Same-Race Cellmate
table(descriptives$samerace.cell)
91/133
aggregate(descriptives$indegree, by=list(descriptives$samerace.cell), FUN=mean)
des3 <- ergm(full ~ nodematch("sameraceroom",diff=F) + edges)
summary(des3)
#Nodematch coefficient = 0.4138
exp(0.4138)
#Odds ratio = 1.51***


#Pre-Prison Interracial Friendship 
table(descriptives$pporf)
46/133
aggregate(descriptives$indegree, by=list(descriptives$pporf), FUN=mean)
des4 <- ergm(full ~ nodematch("pporf",diff=F) + edges)
summary(des4)
#Nodematch coefficient = 0.02738
exp(0.02738)
#Odds ratio = 1.03


#Age
min(descriptives$ageyears)
max(descriptives$ageyears)
mean(descriptives$ageyears)
sd(descriptives$ageyears)
cor.test(descriptives$ageyears, descriptives$indegree)
Moran.I(descriptives$ageyears, getalong.mat)

#Prison Tenure
min(descriptives$yearsinc)
max(descriptives$yearsinc)
mean(descriptives$yearsinc)
sd(descriptives$yearsinc)
cor.test(descriptives$yearsinc, descriptives$indegree)
Moran.I(descriptives$yearsinc, getalong.mat)

#Social Belongingness
min(descriptives$socialbel)
max(descriptives$socialbel)
mean(descriptives$socialbel)
sd(descriptives$socialbel)
cor.test(descriptives$socialbel, descriptives$indegree)
Moran.I(descriptives$socialbel, getalong.mat)


#Days on Unit (divided by 100)
min(descriptives$daysonunit)
max(descriptives$daysonunit)
mean(descriptives$daysonunit)
sd(descriptives$daysonunit)
cor.test(descriptives$daysonunit, descriptives$indegree)
Moran.I(descriptives$daysonunit, getalong.mat)


#Religion
table(descriptives$religion)
32/133
26/133
22/133
34/133
19/133
aggregate(descriptives$indegree, by=list(descriptives$religion), FUN=mean)
des5 <- ergm(full ~ nodematch("Religion",diff=F) + edges)
summary(des5)
#Nodematch coefficient = 0.59453
exp(0.59453)
#Odds ratio = 1.81***


#From the same hometown/city
table(descriptives$hometown)
32/133
26/133
22/133
34/133
19/133
aggregate(descriptives$indegree, by=list(descriptives$religion), FUN=mean)
des5 <- ergm(full ~ nodematch("Religion",diff=F) + edges)
summary(des5)
#Nodematch coefficient = 0.59453
exp(0.59453)
#Odds ratio = 1.81***







#ERGM RUNS


#erg models.
# Set the seed for replicating findings.
set.seed(92915)




#Simple ERGMs

#Baseline model - racial homophily and social distance heterophily terms
a1 <- ergm(full ~ nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) 
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(a1)

#Social distance (indegree)
a2 <- ergm(full ~ nodematch("Race",diff=F) + nodeicov("Soc.Dist.R") 
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(a2)

#Social distance (outdegree)
a3 <- ergm(full ~ nodematch("Race",diff=F) + nodeocov("Soc.Dist.R") 
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(a3)

#Interaction term: racial homophily by ego social distance
a4 <- ergm(full ~ edgecov(egodistance.racehom) + nodematch("Race",diff=F) + nodeocov("Soc.Dist.R") 
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(a4)

#Interaction term: racial homophily by alter social distance
a5 <- ergm(full ~ edgecov(alterdistance.racehom) + nodematch("Race",diff=F) + nodeicov("Soc.Dist.R") 
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(a5)


#TO PLOT THIS INTERACTION:

#Y-Axis = Contribution to odds of a friendship tie
#X-Axis = Social Distance Score (0 to 4)
#Line #1 = Same-race dyad
#Line #2 = Cross-race dyad

marginalplot(a5, var1 = "nodematch.Race", var2 = "nodeicov.Soc.Dist.R", inter = "edgecov.alterdistance.racehom", 
             ci = 0.95, rug = F, 
             point = FALSE, structzeromat = NULL, zeroline = TRUE, 
             color = "black", xlab = NULL, ylab = NULL)


#Bigger ERGMs

#Baseline
b1 <- ergm(full ~ nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=T) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b1)

#Baseline; single racial homophily term (not differentiated by race)
b1a <- ergm(full ~ nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b1a)

#Ego Social Distance by Race Homophily
b2 <- ergm(full ~ edgecov(egodistance.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=T) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b2)

#Ego Social Distance by Race Homophily; single racial homophily term (not differentiated by race)
b2a <- ergm(full ~ edgecov(egodistance.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b2a)


#Alter Social Distance by Race Homophily
b3 <- ergm(full ~ edgecov(alterdistance.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=T) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b3)

#Alter Social Distance by Race Homophily; single racial homophily term (not differentiated by race)
b3a <- ergm(full ~ edgecov(alterdistance.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
           + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
           + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
           + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
           + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
           + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
           + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
           + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
           + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
           + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
           + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b3a)


#Ego "time on unit" by Race Homophily; single racial homophily term (not differentiated by race)
b4a <- ergm(full ~ edgecov(egotime.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
            + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
            + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
            + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
            + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
            + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
            + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
            + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
            + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
            + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
            + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b4a)
#Note: Ego time on unit does not yield a statistically significant interaction term with racial homophily

#Alter "time on unit" by Race Homophily; single racial homophily term (not differentiated by race)
b5a <- ergm(full ~ edgecov(altertime.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
            + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
            + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
            + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
            + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
            + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
            + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
            + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
            + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
            + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
            + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b5a)
#Note: Alter time on unit does not yield a statistically significant interaction term with racial homophily



#Ego "time in prison" by Race Homophily; single racial homophily term (not differentiated by race)
b6a <- ergm(full ~ edgecov(egoprisontime.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
            + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
            + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
            + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
            + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
            + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
            + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
            + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
            + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
            + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
            + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b6a)
#Note: Ego time in prison does not yield a statistically significant interaction term with racial homophily

#Alter "time in prison" by Race Homophily; single racial homophily term (not differentiated by race)
b7a <- ergm(full ~ edgecov(alterprisontime.racehom) + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
            + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
            + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
            + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
            + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
            + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
            + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
            + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
            + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
            + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
            + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(b7a)
#Note: Alter time in prison does not yield a statistically significant interaction term with racial homophily








#Heterophilous Network ERGM
#Model without interaction term 
hetero1 <- ergm(het ~ absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
                + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
                + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
                + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
                + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
                + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
                + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
                + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
                + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
                + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
                + edges + mutual + intransitive + gwesp(decay = .25, fixed = T)
                ,	constraints=~fixallbut(constraint.race.heterophily.net) )
summary(hetero1)








#FINAL MODELS: As of 10/12/18


#Model without interaction term 
Final.1 <- ergm(full ~ nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
            + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
            + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
            + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
            + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
            + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
            + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
            + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
            + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
            + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
            + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(Final.1)

#Alter Social Distance by Race Homophily; single racial homophily term (not differentiated by race)
Final.2 <- ergm(full ~ edgecov(alterdistance.racehom) + nodematch("Race",diff=F) + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
                + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
                + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
                + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
                + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
                + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
                + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
                + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
                + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
                + nodematch("Religion",diff=F) + nodematch("City",diff=F)              
                + edges + mutual + intransitive + gwesp(decay = .25, fixed = T))
summary(Final.2)




summary(Final.2)
#Coefficients:
#Same Race: 0.5704
#Alter SD (indegree nodecov term for social distance): -0.2699
#Alter SD x Same Race: 0.2465

#Standard Errors:
#Same Race: 0.1214
#Alter SD: 0.118
#Alter SD x Same Race: 0.1281

#Mean Social Distance
mean(full %v% "Soc.Dist.R")
#0.72

#To show covariance of ERGM terms
b3a$est.cov
#Covariance of Alter SD and Same Race = 6.603392e-06



obj_OGOmod <- function(vi,vj) { 
  betas[1]*(vi-v_av) + 
    betas[2]*(vj) +
    betas[3]*(vi-v_av)*(vj)  }
calcSumSE <- function(v1) {
  (varB1 +
     (v1-v_av)**2 * varB3 + 
     2 * (v1-v_av) * covB1B3
  )**.5
}

v_av <- 0.72    # mean social distance
betas <- c(-0.2699, 0.5704, 0.2465)    # alter sd, same race, alter sd x same race
vv1 <- 0:4  # Define the values of sd for which the table is to be given
vv2 <- 0:1  # Define the values of same race
sel_tab <- outer((vv1), (vv2), obj_OGOmod)  # calculate the table
round(sel_tab,3)  # round the values to increase readability

par(mar=c(6,4,1,1))
plot(0:4, sel_tab[,1], ylim=c(-1,1), type='l', ylab='Contribution to Network Function',xlab='Alter Social Distance')
lines(0:4, sel_tab[,2], lty=2)
legend(2, 1, c('Same Race','Different Race'), lty=c(2,1))


# calculate CIs for SD by same race plot
# This involves squaring the standard errors for each estimate
#Same Race standard error = 0.1214
0.1214*0.1214
varB1 <- 0.015  # same race
#Alter SD standard error = 0.118
0.118*0.118
varB2 <- 0.014  # SD alter
#Alter SD x Same Race error = 0.1281
0.1281*0.1281
varB3 <- 0.0164  # same race by SD alter
covB1B3 <- 0.000066   # covariance of same race and SD 

vv1 <- seq(0, 4, by=.1)  # Define the values of social distance for which the table is to be given
vv2 <- 0:1  # Define the values of same race
v_av <- 0.72   # mean social distance
sel_tab <- outer((vv1), (vv2), obj_OGOmod)  # calculate the table
SEs <- calcSumSE(vv1)

predframe1 <- data.frame(OGO=vv1, NetCon=sel_tab[,1], 
                         lwr=sel_tab[,1]-1.96*SEs,
                         upr=sel_tab[,1]+1.96*SEs )

predframe2 <- data.frame(OGO=vv1, NetCon=sel_tab[,2], 
                         lwr=sel_tab[,2]-1.96*SEs,
                         upr=sel_tab[,2]+1.96*SEs )

p1 <- ggplot(predframe1, aes(OGO,NetCon)) +
  geom_ribbon(data=predframe2, aes(ymin=lwr,ymax=upr),alpha=.8, fill='limegreen') +
  geom_ribbon(data=predframe1, aes(ymin=lwr,ymax=upr),alpha=.7, fill='yellow') +
  geom_line(data=predframe1) +
  geom_line(data=predframe2, lty=2) +
  scale_x_continuous("Social Distance", expand=c(0,0)) +
  scale_y_continuous("Contribution to Network Function") +
  annotate("text", x=1.7, y=.4, label="Same Race/Ethnicity Friends", cex=5) +
  annotate("text", x=1.7, y=-.6, label="Cross Race/Ethnicity Friends", cex=5) 
# annotate("text", x=4.2, y=.43, label="Same Race/Ethnicity Friends", cex=5) +
# annotate("text", x=4.4, y=-.13, label="Cross Race/Ethnicity Friends", cex=5)
p1 + theme_bw() + theme(text=element_text(size=15))

















#David Code (shared with me on 9/22/18) for plot construction:

#library('gplots')
#library('lattice')
#library('openxlsx')
#library(ggplot2)


# OGO moderating same race effect 
#  This calculates the predicted values. Change to match your model specification.
obj_OGOmod <- function(vi,vj) { 
betas[1]*(vi-v_av) + 
betas[2]*(vj) +
betas[3]*(vi-v_av)*(vj)  }
calcSumSE <- function(v1) {
(varB1 +
(v1-v_av)**2 * varB3 + 
2 * (v1-v_av) * covB1B3
)**.5
}

v_av <- 4    # mean OGO
betas <- c(.189, .262, -.214)    # ego OGO, same race, ego OGO x same race
vv1 <- 1:5  # Define the values of OGO for which the table is to be given
vv2 <- 0:1  # Define the values of same race
sel_tab <- outer((vv1), (vv2), obj_OGOmod)  # calculate the table
round(sel_tab,3)  # round the values to increase readability

par(mar=c(6,4,1,1))
plot(1:5, sel_tab[,1], ylim=c(-.6,.4), type='l', ylab='Contribution to Network Function',xlab='Ego Intergroup Contact Attitude')
lines(1:5, sel_tab[,2], lty=2)
legend(3.5, -.4, c('Same Race','Different Race'), lty=c(2,1))

# calculate CIs for OGO by same race plot
#varB1 <- .00106  # same race: .00106
#varB2 <- .00346  # OGO ego [M]: .00346
#varB3 <- .00753  # same race by OGO ego: .00753
#covB1B3 <- -.00035   # covariance of same race and OGO: -.00035
varB1 <- .0011  # same race: effect 26
varB2 <- .004  # OGO ego [M]: effect 14
varB3 <- .0077  # same race by OGO ego: effect 33
covB1B3 <- .00033   # covariance of same race and OGO: -.00035
vv1 <- seq(1, 5, by=.1)  # Define the values of OGO for which the table is to be given
vv2 <- 0:1  # Define the values of same race
v_av <- 4   # mean OGO
sel_tab <- outer((vv1), (vv2), obj_OGOmod)  # calculate the table
SEs <- calcSumSE(vv1)

predframe1 <- data.frame(OGO=vv1, NetCon=sel_tab[,1], 
lwr=sel_tab[,1]-1.96*SEs,
upr=sel_tab[,1]+1.96*SEs )

predframe2 <- data.frame(OGO=vv1, NetCon=sel_tab[,2], 
lwr=sel_tab[,2]-1.96*SEs,
upr=sel_tab[,2]+1.96*SEs )

p1 <- ggplot(predframe1, aes(OGO,NetCon)) +
  geom_ribbon(data=predframe2, aes(ymin=lwr,ymax=upr),alpha=.8, fill='limegreen') +
  geom_ribbon(data=predframe1, aes(ymin=lwr,ymax=upr),alpha=.7, fill='yellow') +
  geom_line(data=predframe1) +
  geom_line(data=predframe2, lty=2) +
  scale_x_continuous("Social Distance", expand=c(0,0)) +
  scale_y_continuous("Contribution to Network Function") +
  annotate("text", x=1.7, y=.4, label="Same Race/Ethnicity Friends", cex=5) +
  annotate("text", x=1.7, y=-.6, label="Cross Race/Ethnicity Friends", cex=5) 
# annotate("text", x=4.2, y=.43, label="Same Race/Ethnicity Friends", cex=5) +
# annotate("text", x=4.4, y=-.13, label="Cross Race/Ethnicity Friends", cex=5)
p1 + theme_bw() + theme(text=element_text(size=15))


p1 <- ggplot(predframe1, aes(OGO,NetCon)) +
  # geom_ribbon(data=predframe2, aes(ymin=lwr,ymax=upr),alpha=.8, fill='limegreen') +
  # geom_ribbon(data=predframe1, aes(ymin=lwr,ymax=upr),alpha=.7, fill='yellow') +
  geom_ribbon(data=predframe2, aes(ymin=lwr,ymax=upr),alpha=.8, fill='gray60') +
  geom_ribbon(data=predframe1, aes(ymin=lwr,ymax=upr),alpha=.7, fill='gray80') +
  geom_line(data=predframe1) +
  geom_line(data=predframe2, lty=2) +
  scale_x_continuous("Intergroup Contact Attitude", expand=c(0,0)) +
  scale_y_continuous("Contribution to Network Function") +
  annotate("text", x=1.7, y=.4, label="Same Race/Ethnicity Friends", cex=4) +
  annotate("text", x=1.7, y=-.6, label="Cross Race/Ethnicity Friends", cex=4) 
# annotate("text", x=4.2, y=.43, label="Same Race/Ethnicity Friends", cex=5) +
# annotate("text", x=4.4, y=-.13, label="Cross Race/Ethnicity Friends", cex=5)
p1 + theme_bw() + theme(text=element_text(size=12))
