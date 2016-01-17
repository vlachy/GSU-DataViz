###############################################################################
# Try out the content 
# 
###############################################################################
setwd("~/Dropbox/comp/GSU/GSU-DataViz/")
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)



###############################################################################
# Set up
###############################################################################



colNm <- c("id","name","borrough","bld","street",
		"zip","phone","cuisine","inspect.dt","action",
		"violation.cd","violation.desc","critical","score","grade",
		"grade.dt","record.dt","inspection.type")
colCl <- rep("character",times=length(colNm)); names(colCl) <- colNm
colCl["score"] <- "numeric"
df <- read.csv("../inspection.csv",col.names=colNm,colClasses=colCl,na.strings="")

Hmisc::describe(df) #To overcome masking

#----------------------------
#Recode actions and violations
#----------------------------
table(df$action)
df$action <- factor(df$action)
levels(df$action) <- c("closed","reclosed","reopened","no violations","violations")

#How do violations match to violation codes?
vcodes <- unique(df[,c("violation.cd","violation.desc")])
table(duplicated(vcodes$violation.cd)) #1 duplicated
table(duplicated(vcodes$violation.desc)) #2 duplicated
#Which codes are duplicated
cd.dup <- vcodes[duplicated(vcodes$violation.cd),"violation.cd"]
subset(vcodes,violation.cd %in% cd.dup) #These two are equivalent
vcodes <- subset(vcodes,!duplicated(violation.cd))
#Which violations are duplicated
v.dup <- vcodes[duplicated(vcodes$violation.desc),"violation.cd"]
subset(vcodes,violation.cd %in% v.dup)
subset(df,violation.cd %in% v.dup) #These all (22F, 22G) correspond to
# initial inspection/reinspection on cycle inspection ro reinspection. Interesting. Keep them
vcodes <- subset(vcodes,!is.na(violation.cd))
rownames(vcodes) <- vcodes$violation.cd
#Now we can use vcodes as a violation dictionary and remove the entire description from
# the original data
df$violation.desc <- NULL #We could use textsummarization through LSAfun but pain to install

#By the way, looking at some of the violation descriptions...
#...being an inspector sounds like sometimes a horror job!

#----------------------------
#Fix the dates
#----------------------------
library(lubridate)

for(col in paste0(c("inspect","grade","record"),".dt")) df[,col] <- mdy(df[,col])
#Only one record dt: 8/14/2015... Probably the data when the data set was created.
# Not very informative...
df$record.dt <- NULL

#Looking at describe: some outliers in Inspection date?
table(year(df$inspect.dt))
#What are the ones from 2010?
subset(df,year(inspect.dt)==2010) #These look legit...
sample_n(subset(df,year(inspect.dt)==1900),30)
#No idea what is going on there. No information except possible the restaurant name
# -> exclude
df <- subset(df,year(inspect.dt) != 1900) 
#Some of them had na in names, does that make sense?
table(is.na(df$name)) #OK, the rest is fine.

#----------------------------
#Inspection type: split the two parts
#----------------------------

library(stringr)
df <- separate(df,inspection.type,into=c("reason","timing"),sep="/")
df$reason <- str_trim(df$reason)
df$timing <- str_trim(df$timing)
table(df$reason,df$timing)


#----------------------------
#ids vs names?
#---------------------------
length(unique(df$id))
length(unique(df$name))
ids.by.name <- data.frame( df %>% group_by(name) %>%  summarize(n.id=n_distinct(id)))
subset(ids.by.name,n.id==17) #This makes sense. These are chains
subset(ids.by.name,n.id==215) #Ditto
subset(ids.by.name,n.id==9)
subset(ids.by.name,n.id==3) #These are probably either common names or small chains.
# E.g., Billy's bakery, Chen's garden.
subset(df,name=="PINKBERRY") #And different ids are on different addresses... That says it all.
#So ids are identifiers of the establishment.

#----------------------------
#There is a point when you end up looking for the dictionary...
# E.g., how is "score" coded? What does "grade" stand for?
#----------------------------
#https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j
#"Score": "Total Score for a particular inspection. If there was adjudication a judge may reduce the
# total points for the inspection and this field will have the update amount."
#Googling, "How we score and grade" (see the DataViz folder)
#Lower score is better, grade is derived from the score, for particular inspections.
#For each violations, one gets a certain number of score points, the more the worse.
table(df$score)
#Ok, so what does -1 stand for?
subset(df,score==-1)
violations.encountered <- subset(df,score==-1)$violation.cd
vcodes[violations.encountered,]
#Nothing special here. Notably, all these events are violations. Probably, they just didn't give
# a score.
df$score[df$score==-1] <- NA_integer_

#----------------------------
#Explore the cycles of inspections
# Also w.r.t. timing and violations.
#----------------------------
df <- arrange(df,id,inspect.dt)
#Group by inspection
dfi <- ddply(df,c("id","inspect.dt"), function(insp){
			insp[1,c("name","cuisine","action","score","reason","timing")]
		})
dfi <- arrange(dfi,id,inspect.dt)

#Also: Only look at the cycle inspections?
dfid <- data.frame(dfi %>% group_by(id) %>% summarize(n.insp=n(),
				n.cycle=sum(reason=="Cycle Inspection"),
				ever.closed=any(action=="closed")
						))
qplot(dfid$n.insp)
qplot(dfid$n.cycle)
#We have between 3-8 every year which we would expect if we have 
# a cycle inspection and reinspection every year. But for some places, there are many more!
id.manyInspections <- subset(dfid,n.cycle==18)$id
subset(dfi,id==id.manyInspections) #I see. So this place was closed and then reopened,
# and then apparently the cycle inspections followed more frequently.
#table(dfid$n.cycle,dfid$ever.closed)
#There are still some that have 11 or 16...
id.manyInspections.nonClosed <- subset(dfid,n.cycle==17 & ever.closed==F)$id
subset(dfi,id==id.manyInspections.nonClosed) #They got some very large scores
# and had to face several compliance inspections after that, before getting the scores down...
#It seems that one gets more common inspections if one has poor scores.
#Test: initial inspection follows earlier if the reinspection score was bad.
#Only cycle inspections...
dfic <- subset(dfi,reason=="Cycle Inspection" & timing %in% c("Initial Inspection",
				"Re-inspection"))
#First for that restaurant?
dfic$first <- c(T,dfic$id[-1] != dfic$id[-nrow(dfic)])
dfic$time.elapsed <- c(NA_integer_, diff(dfic$inspect.dt)/ddays(1))
dfic$score.last <- c(NA_integer_,dfic$score[-nrow(dfic)])
dfic[dfic$first,c("time.elapsed","score.last")] <- NA_integer_
#Only look at initial reinspections that already had some preceding inspections
dficr <- subset(dfic,timing=="Initial Inspection" & !first)


library(ggthemes)
theme_set(theme_tufte(base_family="sans")) #Set better plotting...
ggplot(dficr,aes(x=time.elapsed)) + geom_histogram(binwidth=30) + 
		scale_x_continuous(breaks=c(0,365/2, 365, 365*(3/2), 365*2))
#Most inspections after either 1/2 year or one year.
ggplot(dficr,aes(x=score.last,y=time.elapsed)) + geom_point() + geom_smooth()
#This really looks like something stepwise...
m.timeScore <- lm(time.elapsed ~ score.last, dficr)
summary(m.timeScore)
library(segmented)

#Guess the breakpoints
m.seg <- segmented(m.timeScore, seg.Z = ~score.last, psi=list(score.last=c(10,30)))
plot(m.seg) #OK, that does not seem particularly successful... 
#To conclude, we see some relationship between scores and inspection intervals,
# but hard to pinpoint precisely.
rm(m.timeScore,m.seg,dficr)

#Reinspections: How long for it to follow after the initial inspection?
dfir <- subset(dfic,reason=="Cycle Inspection" & timing == "Re-inspection")
qplot(dfir$time.elapsed) + xlim(0,100) #Very early followup
rm(dfic,dfir)

#Consider the score distribution. Just scores from initial inspections?
dfii <- subset(dfi,timing=="Initial Inspection" & reason=="Cycle Inspection")
#Grades according to their guidelines. Note that the establishment may improve the grade
# at the reinspection, but here we are only looking at the first inspections, so this
# is not entirely accurate.
dfii$grade<- "A"
dfii$grade[dfii$score>13] <- "B"
dfii$grade[dfii$score>27] <- "C"
dfii$grade <- factor(dfii$grade,levels=c("C","B","A")) # Just for correct plotting
ggplot(dfii,aes(x=score,fill=grade)) + geom_histogram(breaks=c(0,seq(6.5,55.5,by=7),Inf)) +
		scale_fill_brewer(type="div",palette = 7)
#Note the use of a colorblind-friendly palette.
#See also http://colorbrewer2.org/
save(df,dfi,vcodes,file="dataInterim.RData")
#----------------------------
#Play with the ggplot a bit: See the remaining tasks...
#----------------------------
load(file="dataInterim.RData")
#packageurl <- "https://cran.r-project.org/src/contrib/zipcode_1.0.tar.gz"
#install.packages(packageurl,repos=NULL,type="source")
library(zipcode)
library(ggmap)
data(zipcode)

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
ggmap(map.NY) + geom_point(aes(x=lon,y=lat,color=score,size=n.rest),data=dfiz) +
		scale_color_gradient(low="white",high="red")		

#Unusual reasons for inspection: Plot
table(dfi$reason)
dfiz <- dfi %>% group_by(zip) %>% 
		summarize(
				n.rest = n_distinct(id),
				unusual = sum(!(reason %in% c("Cycle Inspection","Pre-permit (Non-operational)",
											"Pre-permit (Operational)")) )/n.rest,
				lat=first(latitude),
				lon=first(longitude)
		)
ggmap(map.NY) + geom_point(aes(x=lon,y=lat,color=unusual,size=n.rest),data=dfiz) +
		scale_color_gradient(low="white",high="red")		

+++++++++++++++++++++++++++