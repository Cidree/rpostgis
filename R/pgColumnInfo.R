# pgColumnInfo
#' Get information about columns in a PostgreSQL table.
#
#' @title Get information about columns in a PostgreSQL table.
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the lines 
#' geometry (e.g., name = c("schema","table"))
#' @param allinfo logical, Get all information on table? Default is column names, types, nullable, and maximum length of character columns
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return data frame
#' @examples
#' \dontrun{
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' pgColumnInfo(conn,c("schema","table"))
#' }

pgColumnInfo<- function(conn,name,allinfo=FALSE) {
  if (allinfo) {cols<-"*"} else {cols<-"column_name,data_type,is_nullable,character_maximum_length"}
  
  df<-dbGetQuery(conn,paste0("SELECT ",cols," FROM information_schema.columns
                            WHERE table_schema = '",name[1],"' AND table_name = '",name[2],"';"))
  return(df)
  
}