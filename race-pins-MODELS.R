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

final.1 <- ergm( 
  get.along.norank.excluded.net 
  ~ edges + mutual 
  + gwidegree( decay = 0.50, fixed = TRUE ) + gwodegree( decay = 0.25, fixed = TRUE )
  + twopath + gwesp( decay = 1.00, fixed = TRUE )
  + edgecov( egodistance.racehom ) 
  + nodeifactor( "Race", base = 2 ) + nodeofactor( "Race", base = 2 ) + nodematch( "Race", diff=FALSE )
  + nodeicov( "Soc.Dist.R" ) + nodeocov( "Soc.Dist.R" ) + absdiff( "Soc.Dist.R", pow=1 )
#  + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
#  + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
#  + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
#  + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
#  + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
#  + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
#  + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
#  + absdiff("Grade", pow=1) + nodeicov("Grade") + nodeocov("Grade")
#  + absdiff("powerinfluence.indeg.log", pow=1) + nodeicov("powerinfluence.indeg.log") + nodeocov("powerinfluence.indeg.log")
#  + nodematch("Religion",diff=FALSE) 
  + nodematch("City",diff=FALSE),
  control = control.ergm( seed = 92915 )
)
summary( final.1 )



!!!need gof here

update this one!!!

# Alter Social Distance by Race Homophily; single racial homophily term (not differentiated by race)
final.2 <- ergm( 
  get.along.norank.excluded.net 
  ~ edgecov( alterdistance.racehom ) 
  + nodematch("Race",diff=F)
  + absdiff("Soc.Dist.R", pow=1) + nodeicov("Soc.Dist.R") + nodeocov("Soc.Dist.R")
  + nodeifactor("Race", base = 2) + nodeofactor("Race", base = 2) 
  + nodeifactor("race_comfort", base = 3) + nodeofactor("race_comfort", base = 3)
  + nodematch("sameraceroom",diff=F) + nodeicov("sameraceroom") + nodeocov("sameraceroom")
  + nodematch("pporf",diff=F) + nodeicov("pporf") + nodeocov("pporf")
  + absdiff("Age", pow=1) + nodeicov("Age") + nodeocov("Age")
  + absdiff("Time.In", pow=1) + nodeicov("Time.In") + nodeocov("Time.In")
  + absdiff("Soc.Belng", pow=1) + nodeicov("Soc.Belng") + nodeocov("Soc.Belng")
  + absdiff("DOU", pow=1) + nodeicov("DOU") + nodeocov("DOU")
  + nodematch("Religion",diff=F) 
  + nodematch("City",diff=F) 
  + edges + mutual + intransitive + gwesp( decay = .25, fixed = TRUE ),
  control = control.ergm( seed = 92915 )
)
summary( final.2 )












!!!HERE with going through this






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
