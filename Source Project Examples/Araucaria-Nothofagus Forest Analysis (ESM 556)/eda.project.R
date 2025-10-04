#################################################
#General info of database
#site: name of the study site (CM, China Muerta National Reserve)
#n.plot: plot number, 1 to 60 for this site
#cod.sev: H (high), L(low), UN (unburned)
#north: north coordinate
#west: west coordinate
#slope: in degrees
#aspect: in degrees
#elev.m: elevation, meters above sea level
#tran.size: transect size 
#transect: each plot as taken to each cardinal direction. It could be used as subplot
#species: species code, the three first letters of the genus and epithet
#sp.name: scientific name
#origen: species geographical origin, exotic and native 
#veg.cov: cover-abundance of each species in percentage -- there are not categories from Braun-Blanquet
#cov.50N-E-S-W: vegetation cover at subplot level, 50 cm from the ground. Each cardinal direction
#cov.140N-E-S-W: vegetation cover at subplot level, 140 cm from the ground. Each cardinal direction
#inters.sum: intersection of coarse woody debris in the transect (other purpose of the project)

library("plyr")
library("plotrix")
library("vegan")
library("car")
library("carData")
library("dplyr")
library("tidyverse")

db <- read.csv("Projects/ChinaMuerta.csv", header=TRUE, sep=",")
head(db)
str(db)
nrow(db)


#Species richness#
#There were 3 categories for other purpose: 
#bare.gr (bare ground), fwd (fine woody debris), cwd (coarse woody debris)
spp<-subset(db, origen=="native" | origen=="exotic")
spp<-droplevels(spp)
nrow(spp)
head(spp)
str(spp)

#Total species richness  
dim(tapply(spp$origen, spp$species, length))  #66 spp

#Species richness by treatment
ddply(spp, c("site","cod.sev"), function(df)
  return(c(species=length(unique(df$species)))))

#Species richness by subplot
plot.rich<-ddply(spp, c("site","n.plot" ,"cod.sev", "transect"), function(df)
  return(c(species=length(unique(df$species)))))
plot.rich

#spread function
#avoiding to lose subplot without species, and filling them with zeros
dt1<-spread(plot.rich, transect, species, fill=0)
dt1

#gather function
#reorganizing subplots (transects) in one column
dt2<-gather(dt1, "east", "north", "south", "west", key="transect", value=species)
dt2

