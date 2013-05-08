




  N O T E : 

        this file is just Ricks scrap work. 

        You probably want one of the other files. 






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



#  BulkLoadVia_R2neo4j.R

## approx 7 seconds per 1,000

  Artists.NodeInputs <- createNodesFromDT ( DT.Nodes.Concs.artist) 
  DT.Nodes.Concs.artist.Loaded <- cbind(DT.Nodes.Concs.artist, Artists.NodeInputs)
  

output <- batchCreateNodesAndRels(NODES.DT, RELS.DT, nodes.idcol="node", addSerialNumberToRels=TRUE) 
