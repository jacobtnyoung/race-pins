# ============================================================================ #
# RACE PINS 
# build file: this file builds the object for analysis in the race manuscript.


# ----
# clear workspace and call libraries needed

rm( list = ls() )

library( here )     # for reading local file paths
library( readxl )   # for reading excel files
library( sna )      # for working with network data
library( network )  # for working with network data


# ----
# call the base datafile and merge in the variables

# load the networks
load( here( "pins-w1-race-network-data-raw.RData" ) )

# merge additional racial attitude variables
merge_vars <- read_excel( here( "pins-w1-race-data.xlsx" ) )

# ----
# add the logged indegree from the power/influence network

load( here( "pins-w1-all-networks-data.RData" ) )
get.along.norank.excluded.net %v% "powerinfluence.indeg.log" <- log( degree( powerinfluence.excluded.net, cmode = "indegree" ) + 1 )

# ----
# recode the comfort measure into a categorical variable
merge_vars$comfort.x <- merge_vars$comfort
merge_vars$comfort.x [ ( merge_vars$comfort.x ) == "0 Less" ] = 0
merge_vars$comfort.x [ ( merge_vars$comfort.x ) == "1 Same" ] = 1
merge_vars$comfort.x [ ( merge_vars$comfort.x ) == "2 More" ] = 2
merge_vars$comfort.x[ is.na( merge_vars$comfort.x ) ] = 1
merge_vars$comfort.x <- as.numeric( merge_vars$comfort.x )

# create a new data frame object, deleting out the old "comfort level" vector
merge_varsx <- merge_vars[,-3] 

# recode missing values
merge_varsx$SameRaceRoom[ is.na( merge_vars$SameRaceRoom ) ] = 0


# ---- 
# check node order to make sure the attributes can be merged in properly
merge_varsx$ID == rownames( as.sociomatrix( get.along.norank.excluded.net ) )

# merge the variables
get.along.norank.excluded.net     %v% "sameraceroom" <- merge_varsx$SameRaceRoom
get.along.norank.excluded.net     %v% "pporf"        <- merge_varsx$prefrndrace
get.along.norank.excluded.net     %v% "race_comfort" <- merge_varsx$comfort.x
get.along.ex.race.homophily.net   %v% "race_comfort" <- merge_varsx$comfort.x
get.along.ex.race.homophily.net   %v% "pporf"        <- merge_varsx$prefrndrace
get.along.ex.race.homophily.net   %v% "race_comfort" <- merge_varsx$comfort.x
get.along.ex.race.heterophily.net %v% "race_comfort" <- merge_varsx$comfort.x
get.along.ex.race.heterophily.net %v% "pporf"        <- merge_varsx$prefrndrace
get.along.ex.race.heterophily.net %v% "race_comfort" <- merge_varsx$comfort.x


# ----
# recode "social distance" variable
# rescaling it so that 0 is lowest value, 4 is highest
r.soc.dist <- get.along.norank.excluded.net %v% "Soc.Dist"
r.soc.dist <- r.soc.dist - 1

# set missing value to the sample mean
r.soc.dist[ is.na( r.soc.dist ) ] <- round( mean( r.soc.dist, na.rm = TRUE ), 3 )

#add recoded social distance measure to the networks
get.along.norank.excluded.net %v% "Soc.Dist.R" <- r.soc.dist
get.along.ex.race.homophily.net  %v% "Soc.Dist.R" <- r.soc.dist
get.along.ex.race.heterophily.net  %v% "Soc.Dist.R" <- r.soc.dist


# ----
# recode days on unit variable

dou <- get.along.norank.excluded.net %v% "DOU"
dou <- dou / 100

# add recoded days on unit measure to the networks
get.along.norank.excluded.net %v% "DOU" <- dou
get.along.ex.race.homophily.net  %v% "DOU" <- dou
get.along.ex.race.heterophily.net  %v% "DOU" <- dou


# ----
# construct data frame for analyzing survey data
# the following section of code involves extracting node attribute data 
# from the network objects and storing it in a data frame for various analyses

id      <- get.along.norank.excluded.net %v% "id"
rrespct <- get.along.norank.excluded.net %v% "Race.Important.Respect"
rsoc    <- get.along.norank.excluded.net %v% "Race.Important"
race    <- get.along.norank.excluded.net %v% "Race"

# create dummy variables for race
white <- race
black <- race
hisp  <- race

white[ ( white ) > 1 ] = 0
black[ ( black ) < 2 ] = 0
black[ ( black ) > 2 ] = 0
black[ ( black ) > 0 ] = 1
hisp[ ( hisp ) < 3 ] = 0
hisp[ ( hisp ) > 3 ] = 0
hisp[ ( hisp ) > 0 ] = 1

# recode comfort variable
rcomfort <- get.along.norank.excluded.net %v% "race_comfort"
less_comfort <- rcomfort
less_comfort[ ( less_comfort ) > 0 ] = 2
less_comfort[ ( less_comfort ) < 1 ] = 1
less_comfort[ ( less_comfort ) > 1 ] = 0

same_comfort <- rcomfort
same_comfort[ ( same_comfort ) > 1 ] = 0
same_comfort[ ( same_comfort ) < 1 ] = 0

more_comfort <- rcomfort
more_comfort[ ( more_comfort ) < 2 ] = 0
more_comfort[ ( more_comfort ) > 0 ] = 1

# additional variable assignment
sameraceroom <- get.along.norank.excluded.net %v% "sameraceroom"
pporf        <- get.along.norank.excluded.net %v% "pporf"
religious    <- get.along.ex.race.heterophily.net %v% "Religious.Attendance.Now"
soc.bel      <- get.along.ex.race.heterophily.net %v% "Soc.Belng"
grade        <- get.along.ex.race.heterophily.net %v% "Grade"
dep          <- get.along.ex.race.heterophily.net %v% "CESD.Depression"
daysunit     <- get.along.ex.race.heterophily.net %v% "DOU"
time.in      <- get.along.ex.race.heterophily.net %v% "Time.In"
rel          <- get.along.ex.race.heterophily.net %v% "Religion"
rel.id       <- data.frame( id, rel )

# create a data-frame object containing racial attitudes variables and network position variables
racedat <- data.frame( 
  r.soc.dist, rrespct, rsoc, less_comfort, same_comfort, 
  more_comfort, sameraceroom, pporf, white, black, hisp 
  )


# ----
# create data frame for matrix construction
# relevant variables

dist      <- racedat$r.soc.dist
id        <- rel.id$id
time_unit <- daysunit

# change american indian to white
race[ ( race ) == 4 ] = 1

# add revised variables to the FULL network
get.along.norank.excluded.net %v% "Race"    <- race
get.along.norank.excluded.net %v% "rrespct" <- rrespct
get.along.norank.excluded.net %v% "rsoc"    <- rsoc


# ----
# construct dyadic terms for interactions

data <- data.frame( id, dist, race, time_unit, time.in )

# STEPS:
# 1. Create ego/alter race matrices
# 2. Create ego/alter social distance matrices
# 3. Create racial homophily matrix
# 4. Create ego social distance x racial homophily interaction term matrix
# 5. Create alter social distance x racial homophily interaction term matrix

# Step 1: Ego/Alter Race
ego.race <- matrix( data$race, nrow = 133, ncol = 133, byrow=FALSE ) 
rownames( ego.race ) <- id 
colnames( ego.race ) <- id 

alter.race <- matrix( data$race, nrow = 133, ncol = 133, byrow=TRUE ) 
rownames( alter.race ) <- id 
colnames( alter.race ) <- id 

# Step 2: Sender/Receiver Social Distance
ego.sd <- matrix( data$dist, nrow = 133, ncol = 133, byrow=FALSE ) 
rownames( ego.sd ) <- id 
colnames( ego.sd ) <- id 

alter.sd <- matrix( data$dist, nrow = 133, ncol = 133, byrow=TRUE ) 
rownames( alter.sd ) <- id 
colnames( alter.sd ) <- id 

# Step 3: Racial homophily matrix
# logical match on ego and alter race
race.hom <-( ego.race == alter.race )

# multiply by 1 to return numeric value
race.hom <- race.hom * 1 

# Step 4: Ego social distance by racial homophily - interaction term matrix
egodistance.racehom <- ego.sd * race.hom

# Step 5: Alter social distance by racial homophily - interaction term matrix
alterdistance.racehom  <- alter.sd * race.hom


# ----
# constructing dyadic terms for interactions

# time on unit
ego.time <- matrix( data$time_unit, nrow = 133, ncol = 133, byrow=FALSE) 
rownames( ego.time ) <- id 
colnames( ego.time ) <- id 
alter.time <- matrix( data$time_unit, nrow = 133, ncol = 133, byrow=TRUE ) 
rownames( alter.time ) <- id 
colnames( alter.time ) <- id 
egotime.racehom <- ego.time * race.hom
altertime.racehom  <- alter.time * race.hom

# time in prison
ego.prisontime <- matrix( data$time.in, nrow = 133, ncol = 133, byrow=FALSE ) 
rownames( ego.prisontime ) <- id 
colnames( ego.prisontime ) <- id 
alter.prisontime <- matrix( data$time.in, nrow = 133, ncol = 133, byrow=TRUE ) 
rownames( alter.prisontime ) <- id 
colnames( alter.prisontime ) <- id 
egoprisontime.racehom <- ego.prisontime * race.hom
alterprisontime.racehom  <- alter.prisontime * race.hom


# ============================================================================ #
# END OF SCRIPT
# ============================================================================ #