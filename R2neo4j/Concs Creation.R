# This file creates the data.table `Concs`

###   Concs can be created by merging  
###   CAconcerts.skinny  &   CAconcerts
###
###  Try starting from the comma-cleaned version of CAconcerts.skinny
###                        -------------
###


# Grab the cID & aName from CAconcerts.skinny
Concs <- setkey(CAconcerts.skinny[, list(concertID, artistName=as.character(artistName))], "concertID")

# add in the venue, city, state, country & date info from CAconcerts
setkeyIfNot(CAconcerts, "concertID")

Concs[CAconcerts, `:=`(
             venue = as.character(i.venue)
     ,        city = as.character(i.city)
     ,       state = as.character(i.state)
     , concertDate = as.Date(i.show_date_start)
     ,     country = as.character(i.country) 
  )]

