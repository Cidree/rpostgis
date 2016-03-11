# pgInsertGeom
#' Inserts data from a Spatial*DataFrame into a PostgreSQL table (with geometry)
#
#' @title Inserts data from a Spatial*DataFrame into a PostgreSQL table (with geometry)
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the lines 
#' geometry (e.g., name = c("schema","table"))
#' @param sdf A Spatial*DataFrame
#' @param cols Character vector of field names of the PostgreSQL table on which the
#' inserts will be made (excluding the geometry column). If cols=TRUE, 
#' the column names of the Spatial* data frame will be used
#' (This can be used in the case that they exactly match the PostgreSQL columns. Note that
#' the '.' character is not allowed in PostgreSQL column names.)
#' @param geom character, Name of the column in the PostgreSQL table holding
#' the geometry object
#' @param db.na A character string, value to change NAs to during insert (defaults to "NULL")
#' @param multi Logical, if PostGIS geometry column is of Multi* type set to TRUE
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
#' \dontrun{
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' pgInsertGeom(conn,"schema.meuse_data",spdf,cols=TRUE,geom="geom")
#' }

pgInsertGeom<-function(conn,table,sdf,cols=TRUE,geom,db.na="NULL",multi=FALSE) {
  
  if (cols) {cols<-colnames(sdf@data)}
  
  values<-pgInsertizeGeom(sdf,db.na=db.na,multi=multi)
  
  cols2<-paste(c(cols,geom),collapse=',')
  
  dbSendQuery(conn,paste0("Insert into ",table,"(",cols2,") VALUES ",values,";"))
}