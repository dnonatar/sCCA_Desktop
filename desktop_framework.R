## Check if the database exists
library('RSQLite')
if(!file.exists("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/tool.db")){
  stop("Database does not exist!")
}

#Connect to the database
db = dbConnect(SQLite(), dbname = "/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/tool.db")
writeLines("Starting Sparse Canonical Correlation Analysis")

while(TRUE) {
# process incomplete jobs
res <- dbGetQuery(db, "SELECT * from ccajobs where (status=='Incomplete')")
if(nrow(res) > 0){

    row <- dbGetQuery(db, "SELECT file1,file2, name, project, rand, id from ccajobs where (status=='Incomplete') limit 1")
    args = c(row[1,1],row[1,2])
    randname = paste(row[1,5],row[1,6], sep="")
    writeLines(paste("Processing job",randname,sep=": "))


result <- tryCatch({
# Use print.eval=T to get plots output  
source("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/CCA_anlys.R", echo = T, print.eval = T)
#write.csv(x = ccaScores_old, file = outfile)

dbSendQuery(conn = db, sprintf("update ccajobs set status='Complete' where id=%s",row[1,6]))
}, error = function(err) {
	dbSendQuery(conn = db, sprintf("update ccajobs set status='Errored' where id=%s",row[1,6]))

})

## close bracket for if(res)
}
Sys.sleep(1)
## close bracket for while(TRUE) loop
}

dbDisconnect(db)
