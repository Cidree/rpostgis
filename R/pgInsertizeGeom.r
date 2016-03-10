# pgInsertizeGeom
#' Formats an R Spatial*DataFrame as character string insert values for a PostgreSQL insert statement.
#
#' @title Formats an R Spatial*DataFrame as character string insert values for a PostgreSQL insert statement.
#'
#' @param sdf A Spatial*DataFrame
#' @param db.na A character string, value to change NAs to (defaults to "NULL")
#' @param multi Logical, if PostGIS geometry column is of Multi* type set to TRUE
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return Character string which is suitable for pasting into a SQL INSERT statement for PostGIS spatial tables.
#' @examples
#' 
#' library(sp)
#' data(meuse)
#' coords <- SpatialPoints(meuse[, c("x", "y")])
#' spdf<- SpatialPointsDataFrame(coords, meuse)
#' values<-pgInsertizeGeom(sdf=spdf)
#' 
#' \dontrun{
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' 
#' ##Note that the geometry column must always be last in the insert column list
#' dbSendQuery(conn,paste0("INSERT INTO schema.table (col1,col2,col3,geom) VALUES ",values,";"))
#' }

pgInsertizeGeom<- function(sdf,db.na = "NULL",multi=FALSE) {
  
  dat<-sdf@data
  geom999<-writeWKT(sdf,byid=TRUE)
  
  proj<-strsplit(as.character(sdf@proj4string), "[: ]+")[[1]][2]
  
  df<-cbind(dat,geom999)
  df[] <- lapply(df, as.character)
  
  df[is.na(df)]<-"NULL"

  #format rows of data frame
  if (!is.na(proj)) {
    if (multi == TRUE) {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(x[1:length(colnames(df))-1],collapse="','")),
                                  "',ST_Multi(ST_GeomFromText('",x[length(colnames(df))],"',",proj,")))")) 
    } else {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(x[1:length(colnames(df))-1],collapse="','")),
                                        "',ST_GeomFromText('",x[length(colnames(df))],"',",proj,"))"))}
  } else {
    warning("spatial projection is unknown. Use projection(sp) if you want to set it.")
    if (multi == TRUE) {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(x[1:length(colnames(df))-1],collapse="','")),
                                  "',ST_Multi(ST_GeomFromText('",x[length(colnames(df))],"')))"))
    } else {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(x[1:length(colnames(df))-1],collapse="','")),
                                        "',ST_GeomFromText('",x[length(colnames(df))],"'))"))}
  }
  
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  return(d1)
}