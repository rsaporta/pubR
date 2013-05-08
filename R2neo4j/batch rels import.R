DTnodes.mini <- data.table(structure(list(node = c(44154, 7553, 18414, 40392, 21770), type = c("artist", "artist", "artist", "artist", "artist"), name = c("Theodis Easley", "Chelsea Williams", "Howard Ghee", "The Balconies", "Junior League"), id = c("ART044154", "ART007553", "ART018414", "ART040392", "ART021770"), sourceGrp = c("Concs", "Concs", "Concs", "Concs", "Concs")), .Names = c("node", "type", "name", "id", "sourceGrp"), class = c("data.table", "data.frame"), row.names = c(NA, -5L)))

DTrels.mini <- data.table(structure(list(start = c("21770", "21770", "21770", "40392", "7553", "21770", "21770", "7553", "21770", "7553"), end = c(44154, 40392, 7553, 40392, 18414, 40392, 21770, 21770, 7553, 40392),     type = c("played_at", "played_with", "played_with", "played_at",     "is_in", "played_with", "played_at", "played_at", "played_with",     "played_at"), source = c("Concs", "Concs", "Concs", "Concs",     "Concs", "Concs", "Concs", "Concs", "Concs", "Concs"), concertID = c("18011",     "86843", "30094", "58171", "", "26415", "126718", "141799",     "33195", "133523"), concertDate = c("2002-01-25", "2006-08-29",     "2003-05-24", "2004-11-13", "", "2002-12-14", "2009-03-28",     "2010-04-10", NA, "2009-04-17")), .Names = c("start", "end", "type", "source", "concertID", "concertDate"), class = c("data.table", "data.frame"), row.names = c(NA, -10L)))


## batch after previous NODES created 


Example request

POST http://localhost:7474/db/data/batch
Accept: application/json
Content-Type: application/json
[ {
  "method" : "POST",
  "to" : "/node",
  "id" : 0,           <~~~~~~   id
  "body" : {
    "name" : "bob"
  }
}, {
  "method" : "POST",
  "to" : "/node",
  "id" : 1,           <~~~~~~   id
  "body" : {
    "age" : 12
  }
}, {
  "method" : "POST",
  "to" : "{0}/relationships",    <~~~~~~  START
  "id" : 3,                      <~~~~~~  new ID  (needed?  Not sure)
  "body" : {
    "to" : "{1}",                <~~~~~~~ END is inside "body"
    "data" : {                   <~~~~~~~ Properties are inside "data" inside "body"
      "since" : "2010"
    },
    "type" : "KNOWS"             <~~~~~~~ Relationship `type`
  }
}, {
  "method" : "POST",         <~~~~ I believe this is modifying the relationship just created before it. 
  "to" : "/index/relationship/my_rels",  
  "id" : 4,
  "body" : {
    "key" : "since",
    "value" : "2010",
    "uri" : "{3}"
  }
} ]


method = "POST"
to =  "{" <id.just.created>  "}/relationships"
id =  <new node number>
