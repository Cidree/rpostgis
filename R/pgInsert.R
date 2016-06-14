# pgInsert
#' Inserts data from a pgInsertize* object into a PostgreSQL table
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
#' #remove "." from column name
#' colnames(spdf@data)[colnames(spdf@data) == 'dist.m']<-"dist_m"
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
#' pgInsert(conn,c("schema","meuse_data"),pgi=pgi)
#' }

pgInsert<-function(conn,pgi,name=NULL,encoding=NULL) {
  
  if(!is.null(pgi$in.table) & !is.null(name) & !identical(pgi$in.table,name)) {
    stop(paste0("pgi object already has insert table set to (",pgi$in.table,"). Set name=NULL and re=run. 
If you want to insert into a different table, set pgi$in.table<-'schema.different_table'"))
  }
  
  #change encoding if specified
  if(!is.null(encoding)) {pgi$insert.data<-iconv(pgi$insert.data,encoding[1],encoding[2])}
  
  #create table if specified
  if (!is.null(records$db.new.table))
  {
    qt<-dbSendQuery(conn,records$db.new.table)
  }
  
  #set name of table
  if(!is.null(pgi$in.table)) {name<-pgi$in.table}
  
  cols<-pgi$db.cols.insert
  values<-pgi$insert.data
  
  db.cols<-pgColumnInfo(conn,name=name)$column_name
  
  test<-match(cols,db.cols)
  unmatched<-cols[is.na(test)]
  
  if (length(unmatched) > 0) {stop(paste0('The column(s) (',paste(unmatched,collapse=","),') are not in the database table.'))}
  
  cols2<-paste0('("',paste(cols,collapse='","'),'")')
  
  #need to suppress PostgreSQL log??? Below not working
  suppressMessages(try(
      q<-dbSendQuery(conn,paste0("Insert into ",paste(name,collapse='.'),cols2," VALUES ",values,";"))
        ))
  if(exists("qt") & !exists("q")) {
    dbSendQuery(conn,paste0("drop table ",pgi$in.table,";"))
    stop(paste0("Insert failed. Table ",pgi$in.table," was dropped from database."))
  }
  
}