# pgInsert
#' Inserts data from a pgInsertize* object into a PostgreSQL table
#
#' @title Inserts data from a pgInsertize* object into a PostgreSQL table
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), and table or view name for the table holding the lines geometry (e.g., name = c("schema","table"))
#' @param pgi A list of columns to insert to and the formatted insert data (an output object from pgInsertize() or pgInsertizeGeom())
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
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' 
#' # insert data in database table (note that an error will be given if 
#' # all insert columns do not match exactly to database table columns)
#' pgInsert(conn,c("schema","meuse_data"),pgi=pgi)
#' }

pgInsert<-function(conn,table,pgi) {
  
  cols<-pgi$db.cols.insert
  values<-pgi$insert.data
  
  db.cols<-pgColumnInfo(conn,name=(c(table[1],table[2])))$column_name
  
  test<-match(cols,db.cols)
  unmatched<-cols[is.na(test)]
  
  if (length(unmatched) > 0) {stop(paste0('The column(s) (',paste(unmatched,collapse=","),') are not in the database table.'))}
  
  cols2<-paste0("(",paste(cols,collapse=','),")")
  
  dbSendQuery(conn,paste0("Insert into ",paste(table,collapse='.'),cols2," VALUES ",values,";"))
}