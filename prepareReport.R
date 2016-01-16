###############################################################################
# Prepare objects to display in the report
# 
###############################################################################
setwd("~/Dropbox/comp/GSU/GSU-DataViz/")
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

library(zipcode)
library(ggmap)
data(zipcode)


###############################################################################
# Set up
###############################################################################
load(file="dataInterim.RData")
 
#Hmisc::describe(df)

###############################################################################
# Basic descriptive
###############################################################################
# Time span
t.y <- table(year(dfi$inspect.dt),month(dfi$inspect.dt))

# Number of restaurants
n.rest <- length(unique(df$id))
# By borrough...
bors <- table(df[match(dfi$id,df$id),"borrough"])

# Inspections by types
t.insptype <- table(dfi$reason,dfi$timing)

# Actions: Particularly closures
t.actions <- prop.table(table(dfi$action))

# Most common violations

# Plot: Typical inspection cycle
# Plot: Typical reinspection

# Plot: Scores distribution

# Plot: Scores and the number of restaurants by zipcode

# Plot: Unusual reasons for inspection



#TODO: include
# Plots
# Descriptives on the most common violations
# Score distribution?
