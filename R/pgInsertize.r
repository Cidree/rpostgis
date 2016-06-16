# pgInsertize
#' Formats an R data frame for insert into a PostgreSQL table (for use with pgInsert)
#
#' @title Formats an R data frame for insert into a PostgreSQL table (for use with pgInsert)
#'
#' @param df A data frame
#' @param create.table character, schema and table of the PostgreSQL table to create (actual table creation will be 
#' done in later in pgInsert().) Column names will be converted to PostgreSQL-compliant names. Default is NULL (no new table created).
#' @param force.match character, schema and table of the PostgreSQL table to compare columns of data frame with 
#' If specified, only columns in the data frame that exactly match the database table will be kept, and reordered
#' to match the database table. Default is NULL (all columns names will be kept, and in the same order given in the data frame.)
#' @param conn A database connection (required if a table is given in for "force.match" parameter)
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return pgi object, a list containing four character strings- (1) in.table, the table name which will be 
#' created or inserted into, if specifed by either create.table or force.match (else NULL)
#' (2) db.new.table, the SQL statement to create the new table, if specified in create.table (else NULL), 
#' (3) db.cols.insert, a character string of the database column names to make inserts on, and 
#' (4) insert.data, a character string of the data to insert. See examples for 
#' usage within the \code{pgInsert} function.
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
#' #format all columns for insert
#' values<-pgInsertize(df=data)
#' 
#' \dontrun{
#' # insert data in database table (note that an error will be given if all insert columns 
#' # do not match exactly to database table columns)
#' pgInsert(conn,c("schema","table"),pgi=values)
#' 
#' ##
#' #run with forced matching of database table column names
#' values<-pgInsertize(df=data,force.match=c("schema","table"),conn=conn)
#' 
#' pgInsert(conn,c("schema","table"),pgi=values)
#' }

pgInsertize <- function(df,create.table=NULL,force.match=NULL,conn=NULL) {
  
  rcols<-colnames(df)
  replace <- "[+-.,!@$%^&*();/|<>]"
  in.tab<-NULL
  new.table<-NULL
  
  if (!is.null(create.table) & !is.null(force.match)) {
      stop("Either create.table or force.match must be null.")
  }
  
  if (!is.null(create.table)) {

    drv <- dbDriver("PostgreSQL")
    
    message("Making table names DB-compliant (replacing special characters with '_').")
    #make column names DB-compliant
    t.names<-tolower(gsub(replace,"_",rcols))
    colnames(df)<-t.names
    
    in.tab<-paste(create.table,collapse='.')
    #make create table statement
    new.table<-postgresqlBuildTableDefinition(drv,name=create.table,obj=df,row.names=FALSE)
  }
  
  
  if (!is.null(force.match)) {
    
    db.cols<-pgColumnInfo(conn,name=force.match)$column_name
    
    db.cols.match<-db.cols[!is.na(match(db.cols,rcols))]
    db.cols.insert<-db.cols.match
    
    #reorder data frame columns
    df<-df[db.cols.match]
    
    message(paste0(length(colnames(df))," out of ",length(rcols)," columns of the data frame match database table columns and will be formatted for database insert."))
    
    in.tab<-paste(force.match,collapse='.')
    
  } else {
    
    #if new table is created, use new column names, else use same data frame column names
    if (!is.null(create.table)) {
      db.cols.insert<-t.names } else
      { 
        in.tab<-NULL
        db.cols.insert<-rcols
      }
  }
  
  #set NA to user-specified NULL value
  df[] <- lapply(df, as.character)
  df[is.na(df)]<-"NULL"
  
  #format rows of data frame
  d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x,fixed=TRUE),collapse="','")),"')"))
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  #create pgi object
  lis<-list(in.table=in.tab,db.new.table=new.table,db.cols.insert=db.cols.insert,insert.data=d1)
  class(lis)<-"pgi"
  
  return(lis)
}


# print.pgi
#
#' @rdname pgInsertize
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