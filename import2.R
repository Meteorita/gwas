library(RMySQL)

# Load ped 
ped_file <- "/home/mrog/data/stroke/geno/sql/data5.ped"
map_file <- "/home/mrog/data/stroke/geno/sql/data5.map"

message("Loading ", ped_file)
ped <- data.table::fread(ped_file)
message("Loading ", map_file)
map <- data.table::fread(map_file)

# Initiate db connection
mydb <- dbConnect(MySQL(), user='mrog', password='ysrfkkxx', 
                 dbname='gwas', host='localhost')

# Split ped
ped1 <- ped[, 1:6]
ped2 <- ped[, -c(1:6)]

#create table with indexes 
ind <- data.frame(row=1:nrow(map), col= seq(1, by = 2, len = ncol(ped2)/2))

# Loop by rows
for (i in 1:nrow(ind)) {
 
  chr <-  map$V1[ind$row[i]]
  snp <- map$V2[ind$row[i]]
  pos <- map$V4[ind$row[i]]
  message(chr, " ", snp, " ", pos)
 
  #check variant
  q <-
    sprintf("select chr, snp, pos from map where chr=%s and pos=%s", chr, pos)
  res <- dbSendQuery(mydb, q)
  out <- dbFetch(res)
  dbClearResult(res)
  
  #insert variant in map if not exist
  if (nrow(out) == 0) {
    q <-
      sprintf("insert into map (chr, snp, pos) values ('%s', '%s', '%s')",
              chr,
              snp,
              pos)
    res <- dbSendQuery(mydb, q)
    dbClearResult(res)
  }
  
  for (j in 1:nrow(ped2)) {
    a0 <- ped2[[j, ind$col[i], exact = T]]
    a1 <- ped2[[j, ind$col[i] + 1, exact = T]]
    name <- ped1$V2[j]
    message(name)
    q <-
      sprintf("select id from map where chr='%s' and pos='%s'", chr, pos)
    res <- dbSendQuery(mydb, q)
    out <- dbFetch(res)
    dbClearResult(res)
    map_id <- out$id
    q <- sprintf("select id from people where name='%s'", name)
    res <- dbSendQuery(mydb, q)
    out <- dbFetch(res)
    dbClearResult(res)
    people_id <- out$id
   #check geno
    q <-
      sprintf("select id from geno 
              where a0='%s' and a1='%s' and people_id='%s' and map_id='%s'", a0, a1, people_id, map_id)
    res <- dbSendQuery(mydb, q)
    out <- dbFetch(res)
    dbClearResult(res)
    if (nrow(out) == 0) {
      q <- sprintf(
      "insert into geno (a0, a1, people_id, map_id) values ('%s', '%s', '%s', '%s')",
      a0,
      a1,
      people_id,
      map_id
    )
      res <- dbSendQuery(mydb, q)
      dbClearResult(res)
    }
    
     
  }
  
}  

dbDisconnect(mydb)
