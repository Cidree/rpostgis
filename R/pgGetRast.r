# pgGetRast
#' @title Load a raster stored in a PostgreSQL database into R.
#'
#' @param conn A connection object created in RPostgreSQL package.
#' @param table Name of the schema-qualified table in Postgresql holding the raster.
#' @param rast Name of the column in 'table' holding the raster object
#' @param proj Can be set to TRUE to automatically take the SRID for the table in the database. Alternatively, the number of EPSG-specified projection of the geometry (Default is NULL, resulting in no projection.)
#' @param digits numeric, precision for detecting whether points are on a regular grid (a low number of digits is a low precision) - From rasterFromXYZ function (raster package)
#' @param NSEW numeric, clipping box for raster with four arguments (north, south, east, west) indicating the projection-specific limits with which to clip the raster.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return RasterLayer
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',user='user',password='password')
#'
#' pgGetRast(conn,'schema.tablename')
#' pgGetRast(conn,'schema.tablename',proj=4326,digits=9,NSEW=c(55,50,17,12))
#' }

pgGetRast <- function(conn,table,rast='rast',proj=NULL,digits=9, NSEW=c(NULL,NULL,NULL,NULL)){

  if (!is.null(NSEW) && is.null(proj)) {stop('proj is required if a clipping box is given')}

  if (isTRUE(proj)){
    t2<-strsplit(table,".",fixed=TRUE)[[1]]
    proj<-dbGetQuery(conn,paste0("select srid from public.raster_columns where r_table_schema = '",t2[1],"' AND r_table_name = '",t2[2],"';"))$srid
  }

  if (is.null(NSEW)) {trast<-suppressWarnings(dbGetQuery(conn,paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, (gv).val FROM (SELECT ST_PixelAsPolygons(",rast,") as gv FROM ",table,") a;")))}
  else {trast<-suppressWarnings(dbGetQuery(conn,paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, (gv).val FROM (SELECT ST_PixelAsPolygons(ST_Clip(",rast,",ST_SetSRID(ST_GeomFromText('POLYGON((",NSEW[4]," ",NSEW[1],",",NSEW[4]," ",NSEW[2],",
  ",NSEW[3]," ",NSEW[2],",",NSEW[3]," ",NSEW[1],",",NSEW[4]," ",NSEW[1],"))'),",proj,"))) as gv FROM ",table,"
  WHERE ST_Intersects(",rast,",ST_SetSRID(ST_GeomFromText('POLYGON((",NSEW[4]," ",NSEW[1],",",NSEW[4]," ",NSEW[2],",
  ",NSEW[3]," ",NSEW[2],",",NSEW[3]," ",NSEW[1],",",NSEW[4]," ",NSEW[1],"))'),",proj,"))) a;")))
  }

  if(!is.null(proj)) {
    p4s<-CRS(paste0("+init=epsg:",proj))@projargs
    return(rasterFromXYZ(trast,crs=CRS(p4s),digits=digits))
  } else {return(rasterFromXYZ(trast,digits=digits))}
}
