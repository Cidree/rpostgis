# pgInsertize
#' This function takes an R data frame and returns a \code{pgi} list object, which
#' is used in the function \code{pgInsert} to insert rows of the data frame into the database table. (Note
#' that this function does not do any modification of the database, it only prepares the R data frame for insert.)
#' The entire data frame is prepared by default, unless \code{force.match} specifies a database table (along with a database connection \code{conn}),
#' in which case the R column names are compared to the \code{force.match} column names, and only
#' exact matches are formatted to be inserted. A new database table can also be prepared to be
#' created (if so, the actual table is created in \code{pgInsert}) using the \code{create.table} argument. If
#' \code{new.id} is specified, a new sequential integer field is added to the data frame. Note: for inserting
#' Spatial* and Spatial*DataFrames types (from \code{sp} package), use the function \code{pgInsertizeGeom}.
#' @title Formats an R data frame for insert into a PostgreSQL table (for use with pgInsert)
#'
#' @param df A data frame
#' @param create.table character, schema and table of the PostgreSQL table to create (actual table creation will be 
#' done in later in pgInsert().) Column names will be converted to PostgreSQL-compliant names. Default is NULL (no new table created).
#' @param force.match character, schema and table of the PostgreSQL table to compare columns of data frame with 
#' If specified, only columns in the data frame that exactly match the database table will be kept, and reordered
#' to match the database table. Default is NULL (all columns names will be kept, and in the same order given in the data frame.)
#' @param conn A database connection (required if a table is given in for "force.match" parameter)
#' @param new.id character, name of a new sequential integer ID column to be added to the table. 
#' @param alter.names Logical, whether to make column and table names DB-compliant (remove special characters). Defualt is TRUE.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return pgi object, a list containing four character strings- (1) in.table, the table name which will be 
#' created or inserted into, if specifed by either create.table or force.match (else NULL)
#' (2) db.new.table, the SQL statement to create the new table, if specified in create.table (else NULL), 
#' (3) db.cols.insert, a character string of the database column names to insert into, and 
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
#' pgInsert(conn,pgi=values,name=c("schema","table"))
#' 
#' ##
#' #run with forced matching of database table column names
#' values<-pgInsertize(df=data,force.match=c("schema","table"),conn=conn)
#' 
#' pgInsert(conn,pgi=values)
#' }

pgInsertize <- function(df,create.table=NULL,force.match=NULL,conn=NULL,new.id=NULL,alter.names=TRUE) {
  
  if (!is.data.frame(df)) {stop("df must be a data frame.")}
  
  replace <- "[+-.,!@$%^&*();/|<>]"
  in.tab<-NULL
  new.table<-NULL
  
  if (!is.null(create.table) & !is.null(force.match)) {
    stop("Either create.table or force.match must be null.")
  }
  
  #add new ID column if new.id is set
  if (!is.null(new.id)) {
    #check if new.id column name is already in data frame
    if (new.id %in% colnames(df)) {stop(paste0("'",new.id,"' is already a column name in the data frame. Pick a unique name for new.id or leave it null (no new ID created)."))}

    id.num<-1:length(df[,1])
    df<-cbind(id.num,df)
    names(df)[1]<-new.id
  }
  
  #make db compliant names
  if (alter.names) {
    message("Making table names DB-compliant (lowercase and replacing special characters with '_').")
    t.names<-tolower(gsub(replace,"_",colnames(df)))
    colnames(df)<-t.names
    }

  #create new table statement if set  
  if (!is.null(create.table)) {

    drv <- dbDriver("PostgreSQL")
    
    if (length(create.table) == 1) {
    nt<-strsplit(create.table,".",fixed=T)[[1]]} else {nt<-create.table}
    
    if (alter.names) {
    nt<-tolower(gsub(replace,"_",nt))}
    
    #make create table statement
    new.table<-postgresqlBuildTableDefinition(drv,name=nt,obj=df,row.names=FALSE)
    
    in.tab<-paste(create.table,collapse='.')
  }
  
  #match columns to DB table if set
  if (!is.null(force.match)) {
    if (is.null(conn)) {stop("Database connection must be specified when using force.match.")}
    
    db.cols<-pgColumnInfo(conn,name=force.match)$column_name
    if (is.null(db.cols)) {stop(paste0("Database table ",paste(force.match,collapse='.')," not found."))}
    
    rcols<-colnames(df)
    db.cols.match<-db.cols[!is.na(match(db.cols,rcols))]
    db.cols.insert<-db.cols.match
    
    #reorder data frame columns
    df<-df[db.cols.match]
    
    message(paste0(length(colnames(df))," out of ",length(rcols)," columns of the data frame match database table columns and will be formatted for database insert."))
    
    in.tab<-paste(force.match,collapse='.')
    
  } else {
    db.cols.insert<-colnames(df)
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