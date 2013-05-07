Cypher queries examples

## SELECT ALL ARTIST THAT PLAYED NY

START m=node:node_auto_index(id="603") 
MATCH m<-[:ACTS_IN]-actor 
RETURN count(*);

START n=node:node_auto_index(id="603")
RETRUN n


START n=node(*)
MATCH n-[:is_in]->state
WHERE state.name =~ ".*Y$"
RETURN count(*)


