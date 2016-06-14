# pgInsertizeGeom
#' Formats an R Spatial*DataFrame for insert (with geometry) into a PostgreSQL table (for use with pgInsert).
#
#' @title Formats an R Spatial*DataFrame for insert (with geometry) into a PostgreSQL table (for use with pgInsert).
#'
#' @param sdf A Spatial*DataFrame
#' @param geom character string, the name of geometry column in the database table. (defaults to 'geom')
#' @param multi Logical, if PostGIS geometry column is of Multi* type set to TRUE
#' @param force.match character, schema and table of the PostgreSQL table to compare columns of data frame with 
#' If specified, only columns in the data frame that exactly match the database table will be kept, and reordered
#' to match the database table. If NULL, all columns will be kept in the same order given in the data frame.
#' @param conn A database connection (if a table is given in for "force.match" parameter)
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return List containing two character strings- (1) db.cols.insert, a character string of the database column
#' names to make inserts on, and (2) insert.data, a character string of the data to insert. See examples for 
#' usage within the \code{pgInsert} function.
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
#' pgi<-pgInsertizeGeom(spdf,geom="point_geom")
#' 
#' \dontrun{
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' 
#' # insert data in database table (note that an error will be given if all 
#' # insert columns do not match exactly to database table columns)
#' pgInsert(conn,c("schema","meuse_data"),pgi=pgi)
#' }

pgInsertizeGeom<- function(sdf,geom='geom',multi=FALSE,force.match=NULL,conn=NULL) {
  
  dat<-sdf@data
  geom.1<-writeWKT(sdf,byid=TRUE)
  
  rcols<-colnames(dat)
  
  if (!is.null(force.match)) {
    
    db.cols<-pgColumnInfo(conn,name=force.match)$column_name
    
    if (is.na(match(geom,db.cols))) {stop('Geometry column name not found in database table.')}
    
    db.cols.match<-db.cols[!is.na(match(db.cols,rcols))]
    db.cols.insert<-c(db.cols.match,geom)
    
    #reorder data frame columns
    df<-dat[db.cols.match]
    
    message(paste0(length(colnames(df))," out of ",length(rcols)," columns of the data frame match database table columns and will be formatted."))
    
  } else {
    db.cols.insert<-c(rcols,geom)
  }
  
  proj<-strsplit(as.character(sdf@proj4string), "[: ]+")[[1]][2]
  
  df<-cbind(dat,geom.1)
  df[] <- lapply(df, as.character)
  
  #set all NA to NULL
  df[is.na(df)]<-"NULL"
  
  #double all single ' to escape
  #format rows of data frame
  if (!is.na(proj)) {
    if (multi == TRUE) {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                  "',ST_Multi(ST_GeomFromText('",x[length(colnames(df))],"',",proj,")))")) 
    } else {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                        "',ST_GeomFromText('",x[length(colnames(df))],"',",proj,"))"))}
  } else {
    warning("spatial projection is unknown (SRID = 0). Use projection(sp) if you want to set it.")
    if (multi == TRUE) {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                  "',ST_Multi(ST_GeomFromText('",x[length(colnames(df))],"')))"))
    } else {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                        "',ST_GeomFromText('",x[length(colnames(df))],"'))"))}
  }
  
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  lis<-list(db.cols.insert=db.cols.insert,insert.data=d1)
  
  class(lis)<-"pgi"
  return(lis)
}


# print.pgi
#
#' @rdname pgInsertizeGeom
#' @param object A list of class \code{pgi}, output from the pgInsertize() or pgInsertizeGeom() functions from the rpostgis package.
#' @export
print.pgi <- function(pgi) {
  cat('pgi object: PostgreSQL insert object from pgInsertize* function in rpostgis. Use with pgInsert() to insert into database table.')
  cat('\n************************************\n')
  if(!is.null(pgi$in.tab)) {
    cat(paste0('Insert table: ',pgi$in.tab))
    cat('\n************************************\n')
  }
  if(!is.null(pgi$db.new.table)) {
    cat(paste0("SQL to create new table: ",pgi$db.new.table))
    cat('\n************************************\n')
  }
  cat(paste0("Columns to insert into: ",paste(pgi$db.cols.insert,collapse=",")))
  cat('\n************************************\n')
  cat(paste0("Formatted insert data: ",substr(pgi$insert.data,0,1000)))
  if(nchar(pgi$insert.data) > 1000) {cat("........Only the first 1000 characters shown")}
}
