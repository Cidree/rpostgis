# pgInsertize
#' Formats an R data frame as a character string as insert values for a PostgreSQL insert statement.
#
#' @title Formats an R data frame as a character string for use in a PostgreSQL insert statement.
#'
#' @param df A data frame
#' @param db.na A character string, value to change NAs to (defaults to "NULL")
#' @param check character, schema and table of the PostgreSQL table to compare columns of data frame with 
#' If not NULL (default), a list is returned with matching column names and insert string
#' @param conn A database connection (if a table is given in for "check" parameter)
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return Character string which is suitable for pasting into a SQL INSERT statement
#' @examples
#' 
#' \dontrun{
#' #connect to database
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' }
#' 
#' data<-data.frame(a=c(1,2,3),b=c(4,NA,6),c=c(7,'text',9))
#' 
#' #just return insert values
#' values<-pgInsertize(df=data)
#' 
#' \dontrun{
#' dbSendQuery(conn,paste0("INSERT INTO schema.table (col1,col2,col3) VALUES ",values,";"))
#' 
#' #return list of matching columns in destination DB table and insert values
#' values.list<-pgInsertize(df=data,check=c("schema","table"),conn=conn)
#' 
#' #review values.list to check if columns match
#' #insert values in existing database table
#' dbSendQuery(conn,paste0("INSERT INTO schema.table (",values.list$db.cols.insert,") VALUES ",values.list$insert.data,";"))
#' }

pgInsertize <- function(df,db.na = "NULL",check=NULL,conn=NULL) {
  
  if (!is.null(check)) {
    rcols<-colnames(df)
    db.cols<-pgColumnInfo(conn,name=(c(check[1],check[2])))$column_name
    
    r.col.match<-rcols[!is.na(match(rcols,db.cols))]
    r.col.nomatch<-rcols[is.na(match(rcols,db.cols))]
    
    db.cols.insert<-paste(db.cols,collapse=',')
  }
  
  #set NA to user-specified NULL value
  df[] <- lapply(df, as.character)
  df[is.na(df)]<-db.na
  
  #format rows of data frame
  d1<-apply(df,1,function(x) paste0("('",toString(paste(x,collapse="','")),"')"))
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  if (!is.null(check)) {lis<-list(db.cols=db.cols,r.col.match=r.col.match,
                         r.col.nomatch=r.col.nomatch,db.cols.insert=db.cols.insert,insert.data=d1)} else {
                           return(d1)                         
                         }
}