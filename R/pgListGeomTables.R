# pgListGeomTables
#' List tables with geometry columns in the database.
#' 
#' @param conn A PostgreSQL database connection
#' @return A data frame with schema, table, geometry column, and geometry type.
#' @export
#' @examples 
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'                
#' pgListGeomTables(conn)
#' }

pgListGeomTables<-function(conn){
  if(suppressMessages(pgPostGIS(conn))) {
    temp.query<-"SELECT f_table_schema as schema_name, f_table_schema as table_name, f_geometry_column as geom_column, type as geometry_type
    FROM public.geometry_columns;"
    
    tab<-dbGetQuery(conn,temp.query)
  } else {
    stop("PostGIS not enabled on this database.")
  }
  
  return(tab)
}