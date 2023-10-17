# ============================================================================ #
# RACE PINS 
# models file: this file estimates the ergms in the race manuscript.


# ----
# clear workspace and call libraries needed

rm( list = ls() )

library( here )     # for reading local file paths
library( sna )      # for working with network data
library( network )  # for working with network data
library( ergm )     # for the erg models


# ----
# execute the build script.
source( here( "race-pins-BUILD.R" ) )

rm( list = ls()[ !(ls() %in% c( "get.along.norank.excluded.net", "egodistance.racehom", "alterdistance.racehom" ) ) ] )


# ----
# ERGM models

# ego model
ego.model <- ergm( 
  get.along.norank.excluded.net 
  ~ nodematch( "Race", diff=FALSE )
  + absdiff( "Soc.Dist.R" ) + nodeicov( "Soc.Dist.R" ) + nodeocov( "Soc.Dist.R" )
  + edgecov( egodistance.racehom ) 
  + nodeifactor( "Race", levels = c( 1, 3 ) ) + nodeofactor( "Race",levels = c( 1, 3 ) )
  + nodeifactor( "race_comfort", levels = c( 1, 2) ) + nodeofactor( "race_comfort", levels = c( 1, 2) )
  + nodematch( "pporf", diff=FALSE ) + nodeicov( "pporf" ) + nodeocov( "pporf" )
  + absdiff( "Age" ) + nodeicov( "Age" ) + nodeocov( "Age" )
  + absdiff( "Time.In" ) + nodeicov( "Time.In" ) + nodeocov( "Time.In" )
  + absdiff( "Soc.Belng" ) + nodeicov( "Soc.Belng" ) + nodeocov( "Soc.Belng" )
  + absdiff( "DOU" ) + nodeicov( "DOU" ) + nodeocov( "DOU" )
  + nodematch( "Religion", diff=FALSE ) 
  + nodematch( "City", diff=FALSE )
  + edges + mutual
  + gwidegree( decay = 0.50, fixed = TRUE ) + gwodegree( decay = 0.25, fixed = TRUE )
  + intransitive + gwesp( decay = 0.25, fixed = TRUE )
  ,
  control = control.ergm( seed = 92915 )
)
summary( ego.model )


#alter model
alter.model <- ergm( 
  get.along.norank.excluded.net 
  ~ nodematch( "Race", diff=FALSE )
  + absdiff( "Soc.Dist.R" ) + nodeicov( "Soc.Dist.R" ) + nodeocov( "Soc.Dist.R" )
  + edgecov( alterdistance.racehom ) 
  + nodeifactor( "Race", levels = c( 1, 3 ) ) + nodeofactor( "Race",levels = c( 1, 3 ) )
  + nodeifactor( "race_comfort", levels = c( 1, 2) ) + nodeofactor( "race_comfort", levels = c( 1, 2) )
  + nodematch( "pporf", diff=FALSE ) + nodeicov( "pporf" ) + nodeocov( "pporf" )
  + absdiff( "Age" ) + nodeicov( "Age" ) + nodeocov( "Age" )
  + absdiff( "Time.In" ) + nodeicov( "Time.In" ) + nodeocov( "Time.In" )
  + absdiff( "Soc.Belng" ) + nodeicov( "Soc.Belng" ) + nodeocov( "Soc.Belng" )
  + absdiff( "DOU" ) + nodeicov( "DOU" ) + nodeocov( "DOU" )
  + nodematch( "Religion", diff=FALSE ) 
  + nodematch( "City", diff=FALSE )
  + edges + mutual
  + gwidegree( decay = 0.50, fixed = TRUE ) + gwodegree( decay = 0.25, fixed = TRUE )
  + intransitive + gwesp( decay = 0.25, fixed = TRUE )
  ,
  control = control.ergm( seed = 92915 )
)
summary( alter.model )


# ----
# GOFs for estimated models

set.seed( 92915 )

ego.model.gof <- gof( ego.model )
ego.model.gof
plot( ego.model.gof )

alter.model.gof <- gof( alter.model )
alter.model.gof
plot( alter.model.gof )
