# pgInsert
#' This function takes an output object from \code{pgInsertize} or \code{pgInsertizeGeom} and 
#' performs the database insert (and table creation, if specified in the previous functions) on
#' the database. If \code{create.table} or \code{force.match} were not specified in the \code{pgInsertize*}
#' statement, the table to insert into should be specified in \code{name} in this function.
#' If a new table is created but the data insert statement fails, the new table is dropped from the database (a
#' message will be given).
#
#' @title Inserts data from a pgInsertize* object into a PostgreSQL table
#' @param conn A connection object to a PostgreSQL database
#' @param pgi The output PostgreSQL insert object (pgi) created by pgInsertize() or pgInsertizeGeom())
#' @param name character strings specifying a PostgreSQL schema and table name to insert into (e.g., name = c("schema","table")). 
#' If table was specified in the pgInsertize* through create.table or force.match, leave this NULL.
#' @param encoding Character vector of length 2, containing the from/to encodings for the 
#' data (as in the function \code{iconv}. For example, if your dataset contain certain latin characters (e.g., accent marks),
#' and the database is in UTF-8, use \code{encoding = c("latin1","UTF-8")}. Left NULL, no conversion will be done.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return DBIResult
#' @examples
#' 
#' library(sp)
#' data(meuse)
#' coords <- SpatialPoints(meuse[, c("x", "y")])
#' spdf<- SpatialPointsDataFrame(coords, meuse)
#' 
#' #format data for insert
#' pgi<-pgInsertizeGeom(spdf,geom="point")
#' 
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' 
#' # insert data in database table (note that an error will be given if 
#' # all insert columns do not match exactly to database table columns)
#' pgInsert(conn,pgi=pgi,name=c("schema","meuse_data"))
#' }

pgInsert<-function(conn,pgi,name=NULL,encoding=NULL) {
  
  if(!is.null(pgi$in.table) & !is.null(name) & !identical(pgi$in.table,name)) {
    stop(paste0("pgi object already has insert table set to (",pgi$in.table,"). Set name=NULL and re=run. 
                If you want to insert into a different table, set pgi$in.table<-'schema.different_table'"))
  }
  
  #change encoding if specified
  if(!is.null(encoding)) {pgi$insert.data<-iconv(pgi$insert.data,encoding[1],encoding[2])}
  
  #create table if specified
  if (!is.null(pgi$db.new.table))
  {
    qt<-dbSendQuery(conn,pgi$db.new.table)
  }
  
  #set name of table
  if(!is.null(pgi$in.table)) {name<-pgi$in.table}
  
  cols<-pgi$db.cols.insert
  values<-pgi$insert.data
  
  db.cols<-pgColumnInfo(conn,name=name)$column_name
  if (is.null(db.cols)) {stop(paste0("Database table ",paste(name,collapse='.')," not found."))}
  
  test<-match(cols,db.cols)
  unmatched<-cols[is.na(test)]
  
  if (length(unmatched) > 0) {stop(paste0('The column(s) (',paste(unmatched,collapse=","),') are not in the database table.'))}
  
  cols2<-paste0('("',paste(cols,collapse='","'),'")')
  
  ##send insert query
  try(qi<-dbSendQuery(conn,paste0('Insert into ',paste(name,collapse='.'),cols2,' VALUES ',values,';')))
  
  ##drop newly created table if insert fails
  if(!is.null(pgi$db.new.table) & exists("qt") & !exists("qi")) {
    dbSendQuery(conn,paste0('drop table "',pgi$in.table,'";'))
    stop(paste0("Insert failed. Table '",pgi$in.table,"' was dropped from database."))
  } 
  
  if (exists("qi")) {
    print(paste0("Data inserted into table '",paste(name,collapse='.'),"'"))
  } else {
    print("Insert failed.")
  }
}