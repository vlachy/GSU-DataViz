###############################################################################
# Prepare objects to display in the report
# 
###############################################################################
.libPaths(c(.libPaths(),"~/.R"))
## setwd("~/Dropbox/comp/GSU/GSU-DataViz/")
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2,lib.loc="~/.R")
library(lubridate)

library(zipcode)
library(ggmap)
data(zipcode)
library(stringr)

library(ggthemes)
theme_set(theme_tufte(base_family="sans")) #Set better plotting...


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
t.insptype.main <- t.insptype[,c("Initial Inspection","Re-inspection","Compliance Inspection")]
t.insptype.other <- t.insptype[,c("Second Compliance Inspection","Limited Inspection","Reopening Inspection")]

# Actions: Particularly closures
t.actions <- prop.table(table(dfi$action))

# Most common violations
t.viol <- table(df$violation.cd)
names(t.viol) <- vcodes[match(names(t.viol),vcodes$violation.cd),"violation.desc"]
t.viol <- sort(t.viol,decreasing=T)
names(t.viol) <- gsub("[^[:print:]]","",names(t.viol))


# Plot: Typical inspection cycle
dfic <- subset(dfi,reason=="Cycle Inspection" & timing %in% c("Initial Inspection",
				"Re-inspection"))
#First for that restaurant?
dfic$first <- c(T,dfic$id[-1] != dfic$id[-nrow(dfic)])
dfic$time.elapsed <- c(NA_integer_, diff(dfic$inspect.dt)/ddays(1))
dfic$score.last <- c(NA_integer_,dfic$score[-nrow(dfic)])
dfic[dfic$first,c("time.elapsed","score.last")] <- NA_integer_

# Plot: Typical reinspection
p.reinspect <- ggplot(subset(dfic,timing=="Initial Inspection" & !first),
				aes(x=time.elapsed)) + geom_histogram(binwidth=30) + 
		scale_x_continuous(breaks=c(0,365/2, 365, 365*(3/2), 365*2))
rm(dfic)

# Plot: Scores distribution
dfii <- subset(dfi,timing=="Initial Inspection" & reason=="Cycle Inspection")
#Grades according to their guidelines. Note that the establishment may improve the grade
# at the reinspection, but here we are only looking at the first inspections, so this
# is not entirely accurate.
dfii$grade<- "A"
dfii$grade[dfii$score>13] <- "B"
dfii$grade[dfii$score>27] <- "C"
dfii$grade <- factor(dfii$grade,levels=c("C","B","A")) # Just for correct plotting
p.scores <- ggplot(dfii,aes(x=score,fill=grade)) + geom_histogram(breaks=c(0,seq(6.5,55.5,by=7),Inf)) +
		scale_fill_brewer(type="div",palette = 7)
rm(dfii)


# Plot: Scores and the number of restaurants by zipcode
dfi$zip <- df[match(dfi$id,df$id),"zip"]
dfi <- merge(dfi,zipcode,by="zip") #dim(dfi), virtually all retained.

map.NY <- get_map("New York City",zoom=11)

#Aggregate scores by zipcode: Averages vs numbers of restaurants.  Compare...
dfiz <- dfi %>% group_by(zip) %>% filter(reason=="Cycle Inspection", timing=="Initial Inspection") %>%
		summarize(
				score = mean(score),
				n.rest = n_distinct(id),
				lat=first(latitude),
				lon=first(longitude)
		)
p.score.zip <- ggmap(map.NY) + geom_point(aes(x=lon,y=lat,color=score,size=n.rest),data=dfiz) +
		scale_color_gradient(low="white",high="red")		

#Unusual reasons for inspection: Plot
dfiz <- dfi %>% group_by(zip) %>% 
		summarize(
				n.rest = n_distinct(id),
				unusual = sum(!(reason %in% c("Cycle Inspection","Pre-permit (Non-operational)",
											"Pre-permit (Operational)")) )/n.rest,
				lat=first(latitude),
				lon=first(longitude)
		)
p.reason.zip <- ggmap(map.NY) + geom_point(aes(x=lon,y=lat,color=unusual,size=n.rest),data=dfiz) +
		scale_color_gradient(low="white",high="red")

rm(dfiz)
