# pgInsertize
#' Formats an R data frame as a character string as insert values for a PostgreSQL insert statement.
#
#' @title Formats an R data frame as a character string for use in a PostgreSQL insert statement.
#'
#' @param df A data frame
#' @param db.na A character string, value to change NAs to (defaults to "NULL")
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return Character string which is suitable for pasting into a SQL INSERT statement
#' @examples
#' 
#' data<-data.frame(a=c(1,2,3),b=c(4,NA,6),c=c(7,'text',9))
#' values<-pgInsertize(df=data)
#' 
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' dbSendQuery(conn,paste0("INSERT INTO schema.table (col1,col2,col3) VALUES ",values,";"))
#' }

pgInsertize <- function(df,db.na = "NULL") {
  
  #set NA to user-specified NULL value
  df[] <- lapply(df, as.character)
  df[is.na(df)]<-db.na
  
  #format rows of data frame
  d1<-apply(df,1,function(x) paste0("('",toString(paste(x,collapse="','")),"')"))
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  return(d1)
}