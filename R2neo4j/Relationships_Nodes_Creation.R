# Create Nodes & Relationships CSVs



SOURCEGRP <- "Concs"








# ------------------------------------ #
#               EXAMPLE                #
#                                      #
# ------------------------------------ #
# make unique table for each node type #
# ------------------------------------ #
#  artist   :  name,  id,  sourceGrp   #
#  venue    :  name,  id,  sourceGrp   #
#  city     :  name,  id,  sourceGrp   #
#  state    :  name,  id,  sourceGrp   #
#  country  :  name,  id,  sourceGrp   #
# ------------------------------------ #


  # --------------------------------------------------------------------------- #
  #                                                                             #
  #  `NODE.TYPES` is a vector with the following setup:                         #
  #      c( R.name = " neo4j.name ", ...  )                                     #
  #                                                                             #
  #  That is, the   name   of the element corresponds to the column name in R   #
  #           the   value  of the element corresponds to the neo4j node type    #

    NODE.TYPES <-  c(artist="artist", venue="venue", city="city", 
                       state="state", country="country")
  #                                                                             #
  # --------------------------------------------------------------------------- #


  ID.prfx  <- c(artist="ART", venue="VEN", city="CITY", state="STATE", country="CNTRY")
  ID.digs  <- c(artist=6, venue=6, city=5, state=4, country=4)

  .currentNode <<- 0
  createAllNodes(CAmerged, NODE.TYPES, ID.prfx, ID.digs, sourceGrp=SOURCEGRP) 

  # -------------------------------------------------------------------------------------------------------- #


# -------------------------------------------------------- #
#                                                          #
#        CREATE RELATIONAL TABLES                          #
#                                                          #
# -------------------------------------------------------- #

  RELS.DT.Maker.list <- strsplit(c("artist,played_with,artist", "artist,played_at,venue", "venue,is_in,city", "city,is_in,state", "state,is_in,country"), ",")
  RELS.DT.Names      <- sapply(RELS.DT.Maker.list, function(R) do.call(mkDTRelName, as.list(c(R, SOURCEGRP))))

# set to TRUE for debugging
keep.names <- FALSE

# DT.Rel.Concs.artist.played_with.artist
getRelationsWithinColumnByKey(CAmerged, "artist", "played_with", by=c("concertID", "concertDate"), sourceGrp=SOURCEGRP, keep.names=keep.names, verbose=TRUE)

# DT.Rel.Concs.artist.played_at.venue
getRelationsFromRows(CAmerged, "artist", "played_at", "venue", sourceGrp=SOURCEGRP, include=list("concertID", "concertDate"), keep.names=keep.names, verbose=TRUE)

# DT.Rel.Concs.venue.is_in.city
getRelationsFromRows(CAVenueGeo, "venue", "is_in", "city", sourceGrp=SOURCEGRP, add=list(source="Concs"), keep.names=keep.names, verbose=TRUE)

# DT.Rel.Concs.city.is_in.state
getRelationsFromRows(CAVenueGeo, "city", "is_in", "state", sourceGrp=SOURCEGRP, keep.names=keep.names, verbose=TRUE)

# DT.Rel.Concs.state.is_in.country
getRelationsFromRows(CAVenueGeo, "state", "is_in", "country", sourceGrp=SOURCEGRP, keep.names=keep.names, verbose=TRUE)

# ------------------------------------------------------- #


  # --------------------------------------------------- #
  #                                                     #
  #%%           THESE ARE THE FINAL TABLES            %%#
  #                                                     #
  # --------------------------------------------------- #


  # COLLAPSE THE MULTIPLE DT's INTO A SINGLE NODES TABLE & A SINGLE RELS TABLE
  NODES.DT <- do.call(rbind, lapply(mkDTNodesName(names(NODE.TYPES), SOURCEGRP), get))
  RELS.DT  <- combineRelDTs(RELS.DT.Names, verbose=TRUE)


    
  dir.create(as.path(dataDir, "neoForImport"))
  fileOut.nodes <- as.path(dataDir, "neoForImport", "nodes.csv")
  fileOut.rels  <- as.path(dataDir, "neoForImport", "rels.csv" )

  write.table(NODES.DT[, !"node", with=FALSE], fileOut.nodes, row.names=FALSE, fileEncoding = "UTF-8", sep="\t", quote=FALSE)
  write.table(RELS.DT,                         fileOut.rels,  row.names=FALSE, fileEncoding = "UTF-8", sep="\t", quote=FALSE)

                                                                                                            
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


#     #-------------------#
#     
#     
#     
#     # NODES
#      
#     artist   :  name,  id,  source
#     venue    :  name,  id,  source
#     city     :  name,  id,  source
#     state    :  name,  id,  source 
#     country  :  name,  id,  source
#     
#     
#     # RELATIONSHIPS
#     
#     artist1 -[:played_with]-> artist2
#     artist  -[:played_at]->   venue
#     venue     -[:is_in]->     city
#     city      -[:is_in]->     state
#     state     -[:is_in]->     country
#     
#     
#     rule                             type          start     end       ID           DSource      prop:Date
#     *same concertID & distinct*      played_with   artist    artist    concertID    <Concs>      concertDate
#     
#     *same row*                       played_at     artist    venue     concertID    <Concs>      concertDate
#     *same row, venue DB*             is_in         venue     city      <generate>   <Concs>      
#     *same row, venue DB & distinct*  is_in         city      state     <generate>   <Concs>      
#     *same row, venue DB & distince*  is_in         state     country   <generate>   <Concs>      
#     
#     
#     ADD A ROW ID TO THE CONCERTS TABLE
#     CREATE A NODES TABLE, AND GIVE EACH A NODE ID, GRABBING THE ROW ID FROM CONCERTS
#     
#     
#         concertID        artist                   venue         city state concertDate country
#      1:     42469           311        Long Beach Arena   Long Beach    CA  2004-06-22        
#      2:     42469     The Roots        Long Beach Arena   Long Beach    CA  2004-06-22        
#      3:     42469       Medeski        Long Beach Arena   Long Beach    CA  2004-06-22        
#      4:     42469 Martin & Wood        Long Beach Arena   Long Beach    CA  2004-06-22        
#      5:     43353           311 DTE Energy Music Center    Clarkston    MI  2004-07-07        
#      6:     43353     The Roots DTE Energy Music Center    Clarkston    MI  2004-07-07        
#      7:     43353       Medeski DTE Energy Music Center    Clarkston    MI  2004-07-07        
#      8:     43353 Martin & Wood DTE Energy Music Center    Clarkston    MI  2004-07-07        
#      9:     43359           311           Penns Landing Philadelphia    PA  2004-07-09        
#     10:     43359     The Roots           Penns Landing Philadelphia    PA  2004-07-09        
#     11:     43359       Medeski           Penns Landing Philadelphia    PA  2004-07-09        
#     12:     43359 Martin & Wood           Penns Landing Philadelphia    PA  2004-07-09        
#     
#     
#     ###########
#     
#     The format is property_1, property_2, property_3 separated by tabs… and rels.csv:
#     
#      
#     Start   Ende    Type    Property
#     5496772 6842185 FIVE    Property
#     7416995 6166503 FOUR    Property
#     6712458 6853172 THREE   Property
#     1291639 296708  TWO     Property
