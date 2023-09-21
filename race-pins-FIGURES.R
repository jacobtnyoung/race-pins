# ============================================================================ #
# RACE PINS 
# figures file: this file creates the figures for the race paper.

# ----
# clear workspace and call libraries needed

rm( list = ls() )

library( here )     # for reading local file paths
library( sna )      # for working with network data
library( network )  # for working with network data
library( ergm )     # for the erg models
library( lattice )  # for the plots
library( ggplot2 )  # for the plots


# ----
# load the estimated models
load( here( "pins-w1-race-ERGM-RESULTS.RData" ) )


# ----
# define functions to create plot values
obj_OGOmod <- function( vi,vj ) { 
  betas[1]*( vi-v_av ) + 
    betas[2]*( vj ) +
    betas[3]*( vi-v_av ) * ( vj )  }
calcSumSE <- function( v1 ) {
  ( varB1 +
      ( v1-v_av )**2 * varB3 + 
      2 * ( v1-v_av ) * covB1B3
  )**.5
}


# ###### #
# FIGURE 3

# check model output
summary( ego.model )

# define standard errors
ego.se <- sqrt( diag( ego.model$covar ) )

# mean social distance
v_av <- mean( get.along.norank.excluded.net %v% "Soc.Dist.R" )

# model estimates
betas <- as.vector( c(
  ego.model$coefficients["nodeocov.Soc.Dist.R"],         # ego SD
  ego.model$coefficients["nodematch.Race"],              # same race
  ego.model$coefficients["edgecov.egodistance.racehom"]  # ego SD x same race
  ) )     

# Define the values of SD for which the table is to be given
vv1 <- 0:4  

# Define the values of same race
vv2 <- 0:1  

# calculate the table
sel_tab <- outer( ( vv1 ), ( vv2 ), obj_OGOmod )

# variance for ego SD
varB2 <- as.vector( ego.se["nodeocov.Soc.Dist.R"] )

# variance for same race
varB1 <- as.vector( ego.se["nodematch.Race"] )

# variance for ego SD X same race
varB3 <- as.vector( ego.se["edgecov.egodistance.racehom"] )

# covariance of ego SD and same race
covB1B3 <- as.vector( ego.model$est.cov["nodeocov.Soc.Dist.R", "nodematch.Race"] )

# calculate standard errors
SEs <- calcSumSE( vv1 )

# run functions
predframe1 <- data.frame( OGO=vv1, NetCon=sel_tab[,1], 
                         lwr=sel_tab[,1]-1.96*SEs,
                         upr=sel_tab[,1]+1.96*SEs )

predframe2 <- data.frame( OGO=vv1, NetCon=sel_tab[,2], 
                         lwr=sel_tab[,2]-1.96*SEs,
                         upr=sel_tab[,2]+1.96*SEs )

# plot results
p1 <- ggplot( predframe1, aes( OGO, NetCon ) ) +
  geom_ribbon( data=predframe2, aes( ymin=lwr,ymax=upr ),alpha=.8, fill= "grey60") +
  geom_ribbon( data=predframe1, aes( ymin=lwr,ymax=upr ),alpha=.7, fill= "grey80") +
  geom_line( data=predframe1 ) +
  geom_line( data=predframe2, lty=2 ) +
  scale_x_continuous( "Ego Social Distance", expand=c( 0,0 ) ) +
  scale_y_continuous( "Log-Odds of a Friendship Tie" ) +
  annotate("text", x=1.7, y=.4, label="Intraracial Friendship Tie", cex=5 ) +
  annotate("text", x=1.7, y=-.6, label="Interracial Friendship Tie", cex=5 ) 
p1 + theme_bw() + theme( text=element_text( size=15 ) )


# ###### #
# FIGURE 4

# check model output
summary( alter.model )

# define standard errors
alter.se <- sqrt( diag( alter.model$covar ) )

# mean social distance
v_av <- mean( get.along.norank.excluded.net %v% "Soc.Dist.R" )

# model estimates
betas <- as.vector( c(
  alter.model$coefficients["nodeicov.Soc.Dist.R"],           # alter SD
  alter.model$coefficients["nodematch.Race"],                # same race
  alter.model$coefficients["edgecov.alterdistance.racehom"]  # alter SD x same race
) )     

# Define the values of SD for which the table is to be given
vv1 <- 0:4  

# Define the values of same race
vv2 <- 0:1  

# calculate the table
sel_tab <- outer( ( vv1 ), ( vv2 ), obj_OGOmod )

# variance for alter SD
varB2 <- as.vector( alter.se["nodeicov.Soc.Dist.R"] )

# variance for same race
varB1 <- as.vector( alter.se["nodematch.Race"] )

# variance for alter SD X same race
varB3 <- as.vector( alter.se["edgecov.alterdistance.racehom"] )

# covariance of ego SD and same race
covB1B3 <- as.vector( alter.model$est.cov["nodeicov.Soc.Dist.R", "nodematch.Race"] )

# calculate standard errors
SEs <- calcSumSE( vv1 )

# run functions
predframe1 <- data.frame( OGO=vv1, NetCon=sel_tab[,1], 
                          lwr=sel_tab[,1]-1.96*SEs,
                          upr=sel_tab[,1]+1.96*SEs )

predframe2 <- data.frame( OGO=vv1, NetCon=sel_tab[,2], 
                          lwr=sel_tab[,2]-1.96*SEs,
                          upr=sel_tab[,2]+1.96*SEs )

# plot results
p2 <- ggplot( predframe1, aes( OGO, NetCon ) ) +
  geom_ribbon( data=predframe2, aes( ymin=lwr,ymax=upr ),alpha=.8, fill= "grey60") +
  geom_ribbon( data=predframe1, aes( ymin=lwr,ymax=upr ),alpha=.7, fill= "grey80") +
  geom_line( data=predframe1 ) +
  geom_line( data=predframe2, lty=2 ) +
  scale_x_continuous( "Alter Social Distance", expand=c( 0,0 ) ) +
  scale_y_continuous( "Log-Odds of a Friendship Tie" ) +
  annotate("text", x=1.7, y=.4, label="Intraracial Friendship Tie", cex=5 ) +
  annotate("text", x=1.7, y=-.6, label="Interracial Friendship Tie", cex=5 ) 
p2 + theme_bw() + theme( text=element_text( size=15 ) )




