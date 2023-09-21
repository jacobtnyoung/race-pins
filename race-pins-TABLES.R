# ============================================================================ #
# RACE PINS 
# tables file: this file creates the ergms table for the race paper.

# ----
# clear workspace and call libraries needed

rm( list = ls() )

library( here )      # for reading local file paths
library( sna )       # for working with network data
library( network )   # for working with network data
library( ergm )      # for the erg models
library( stargazer ) # for the tables

# ----
# load the estimated models
load( here( "pins-w1-race-ERGM-RESULTS.RData" ) )


# ----
# first table

stargazer(
  ego.model,
  coef = list( coef( ego.model ) ),
  se = list( sqrt( diag(ego.model$covar ) ) ),
  type = "html",
  out = here( "pins-w1-race-ego-table.doc" ),
  show.se = TRUE,
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  single.row = FALSE,
  no.space=FALSE,
  df=FALSE,
  notes.align = "l",
  digits = 3,
  style="ajs",
  covariate.labels = c( "Race (Match)",
                        "Social Distance-Absolute Difference",
                        "Social Distance-Alter",
                        "Social Distance-Ego",
                        "Ego Social Distance x Race Match",
                        "Race (Alter) White (vs Black)",
                        "Race (Alter) Hispanic (vs Black)",
                        "Race (Ego) White (vs Black)",
                        "Race (Ego) Hispanic (vs Black)",
                        "Interracial Comfort (Alter) Less comfortable around other races (vs More)",
                        "Interracial Comfort (Alter)Same comfort level around other races (vs More)",
                        "Interracial Comfort (Ego) Less comfortable around other races (vs More)",
                        "Interracial Comfort (Ego) Same comfort level around other races (vs More)",
                        "Pre-Prison Interracial Friendship - Match",
                        "Pre-Prison Interracial Friendship - Alter",
                        "Pre-Prison Interracial Friendship - Ego",
                        "Age - Absolute Difference",
                        "Age - Alter",
                        "Age - Ego",
                        "Prison Tenure - Absolute Difference",
                        "Prison Tenure - Alter",
                        "Prison Tenure - Ego",
                        "Social Belongingness - Absolute Difference",
                        "Social Belongingness - Alter",
                        "Social Belongingness - Ego",
                        "Days on Unit (div. by 100) - Absolute Difference",
                        "Days on Unit (div. by 100) - Alter",
                        "Days on Unit (div. by 100) - Ego",
                        "Religion  (Match)",
                        "City (Match)",
                        "Edges",
                        "Mutuality",
                        "GW indegree (decay = 0.50)",
                        "GW outdegree (decay = 0.25)",
                        "Intransitive Triads",
                        "GW ESP (alpha = 0.25)" )
  )


# ----
# second table

stargazer(
  alter.model,
  coef = list( coef( alter.model ) ),
  se = list( sqrt( diag(alter.model$covar ) ) ),
  type = "html",
  out = here( "pins-w1-race-alter-table.doc" ),
  show.se = TRUE,
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  single.row = FALSE,
  no.space=FALSE,
  df=FALSE,
  notes.align = "l",
  digits = 3,
  style="ajs",
  covariate.labels = c( "Race (Match)",
                        "Social Distance-Absolute Difference",
                        "Social Distance-Alter",
                        "Social Distance-Ego",
                        "Alter Social Distance x Race Match",
                        "Race (Alter) White (vs Black)",
                        "Race (Alter) Hispanic (vs Black)",
                        "Race (Ego) White (vs Black)",
                        "Race (Ego) Hispanic (vs Black)",
                        "Interracial Comfort (Alter) Less comfortable around other races (vs More)",
                        "Interracial Comfort (Alter)Same comfort level around other races (vs More)",
                        "Interracial Comfort (Ego) Less comfortable around other races (vs More)",
                        "Interracial Comfort (Ego) Same comfort level around other races (vs More)",
                        "Pre-Prison Interracial Friendship - Match",
                        "Pre-Prison Interracial Friendship - Alter",
                        "Pre-Prison Interracial Friendship - Ego",
                        "Age - Absolute Difference",
                        "Age - Alter",
                        "Age - Ego",
                        "Prison Tenure - Absolute Difference",
                        "Prison Tenure - Alter",
                        "Prison Tenure - Ego",
                        "Social Belongingness - Absolute Difference",
                        "Social Belongingness - Alter",
                        "Social Belongingness - Ego",
                        "Days on Unit (div. by 100) - Absolute Difference",
                        "Days on Unit (div. by 100) - Alter",
                        "Days on Unit (div. by 100) - Ego",
                        "Religion  (Match)",
                        "City (Match)",
                        "Edges",
                        "Mutuality",
                        "GW indegree (decay = 0.50)",
                        "GW outdegree (decay = 0.25)",
                        "Intransitive Triads",
                        "GW ESP (alpha = 0.25)" )
)

