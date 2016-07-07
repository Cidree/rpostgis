# pgInsertizeGeom
#' This function takes an R \code{sp} object (Spatial* or Spatial*DataFrame) and returns a \code{pgi} list object, which
#' is used in the function \code{pgInsert} to insert geometries/data frame rows of the object into the database table. (Note
#' that this function does not do any modification of the database, it only prepares the data for insert.)
#' If given a \code{Spatial*DataFrame}, the entire data frame is prepared by default, unless \code{force.match} specifies a database table (along with a database connection \code{conn}),
#' in which case the R column names are compared to the \code{force.match} column names, and only
#' exact matches are formatted to be inserted. A new database table can also be prepared to be
#' created (if so, the actual table is created in \code{pgInsert}) using the \code{create.table} argument. If
#' \code{new.id} is specified, a new sequential integer field is added to the data frame. For \code{Spatial*}-only objects (no data frame),
#' a new.id is created by default with name "gid". If the R package \code{wkb} is installed, this function uses \code{writeWKB} to translate the
#' geometries (faster for large datasets), otherwise the \code{rgeos} function \code{writeWKT} is used.
#' Note: for inserting regular R data frames, use the function \code{pgInsertize}.
#
#' @title Formats an R sp object (Spatial* or Spatial*DataFrame) for insert (with geometry) into a PostgreSQL table (for use with pgInsert).
#'
#' @param sdf A Spatial* or Spatial*DataFrame
#' @param geom character string, the name of geometry column in the database table. (existing or to be created; defaults to 'geom')
#' @param create.table character, schema and table of the PostgreSQL table to create (actual table creation will be 
#' done in later in pgInsert().) Column names will be converted to PostgreSQL-compliant names. Default is NULL (no new table created).
#' @param force.match character, schema and table of the PostgreSQL table to compare columns of data frame with 
#' If specified, only columns in the data frame that exactly match the database table will be kept, and reordered
#' to match the database table. If NULL, all columns will be kept in the same order given in the data frame.
#' @param conn A database connection (if a table is given in for "force.match" parameter)
#' @param multi Logical, if PostGIS geometry column is/will be of Multi* type set to TRUE
#' @param new.gid character, name of a new sequential integer ID column to be added to the table. For Spatial*DataFrames, the default is no
#' new gid column. For spatial objects with no data frame (e.g., SpatialPolygons), a "gid" unique integer column is inserted by default.
#' @param alter.names Logical, whether to make column and table names DB-compliant (remove special characters). Defualt is TRUE.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return pgi object, a list containing four character strings- a list containing four character strings- (1) in.table, the table name which will be 
#' created or inserted into, if specifed by either create.table or force.match (else NULL)
#' (2) db.new.table, the SQL statement to create the new table, if specified in create.table (else NULL), 
#' (3) db.cols.insert, a character string of the database column names to insert into, and 
#' (4) insert.data, a character string of the data to insert. See examples for 
#' usage within the \code{pgInsert} function.
#' @examples
#' 
#' library(sp)
#' data(meuse)
#' coords <- SpatialPoints(meuse[, c("x", "y")])
#' spdf<- SpatialPointsDataFrame(coords, meuse)
#' 
#' #format data for insert
#' pgi.new<-pgInsertizeGeom(spdf,geom="point_geom",create.table=c("schema","table"),new.gid="pt_gid")
#' print(pgi.new)
#' 
#' \dontrun{
#'
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#' 
#' # insert data in database table (note that an error will be given if all 
#' # insert columns do not have exactly matching database table columns)
#' pgInsert(conn,pgi=pgi.new)
#' 
#' 
#' # Inserting into existing table
#' pgi.existing<-pgInsertizeGeom(spdf,geom="point_geom",force.match=c("schema","table"),conn=conn)
#' # A warning message is given, since the "dist.m" column is not found in the database table 
#' # (it was changed to "dist_m" in pgi.new to make name DB-compliant). 
#' # All other columns are prepared for insert.
#' print(pgi.existing)
#' 
#' pgInsert(conn,pgi=pgi.existing)
#' }

pgInsertizeGeom<- function(sdf,geom='geom',create.table=NULL,multi=FALSE,force.match=NULL,conn=NULL,new.gid=NULL,alter.names=TRUE) {
  
  #load wkb library if available
  wkb.t<-suppressWarnings(require("wkb",quietly = TRUE))
  
  #if spatial*dataframe, extract data frame
  dat<-data.frame()
  try(dat<-sdf@data,silent=TRUE)
  gid<-1:length(sdf)
  
  #if data frame doesn't exist (not a spatial*dataframe object), populate it with seq. id
  if(length(colnames(dat)) == 0) {
    dat<-data.frame(gid=gid)
    if (!is.null(new.gid)) {
      names(dat)[1]<-new.gid
    }
    message(paste0("No data frame; creating sequential id column (",colnames(dat),")."))
  } else { #else if it exists, add seq. id if new.gid is not null
    if (!is.null(new.gid)) {
      if (new.gid %in% colnames(dat)) {stop(paste0("'",new.gid,"' is already a column name in the data frame. Pick a unique name for new.gid or leave it null (no new ID created)."))}
      dat<-cbind(gid,dat)
      names(dat)[1]<-new.gid
    }
  }
  
  replace <- "[+-.,!@$%^&*();/|<>]"
  in.tab<-NULL
  new.table<-NULL
  
  #make db compliant names
  if (alter.names) {
    message("Making table names DB-compliant (lowercase and replacing special characters with '_').")
    #make column and table names DB-compliant
    t.names<-tolower(gsub(replace,"_",colnames(dat)))
    colnames(dat)<-t.names
    geom<-tolower(gsub(replace,"_",geom))
  }
  
  #extract proj
  proj<-NA
  try(proj<-showEPSG(as.character(sdf@proj4string)),silent=TRUE)
  if(!is.na(proj) & proj == "OGRERR_UNSUPPORTED_SRS") {proj<-NA}
  #
  
  if (!is.null(create.table) & !is.null(force.match)) {
    stop("Either create.table or force.match must be null.")
  }
  
  if (!is.null(create.table)) {
    
    drv <- dbDriver("PostgreSQL")
    
    if (length(create.table) == 1) {
      nt<-strsplit(create.table,".",fixed=T)[[1]]} else {nt<-create.table}
    
    if (alter.names) {
    nt<-tolower(gsub(replace,"_",nt))}

    #make create table statement
    new.table<-postgresqlBuildTableDefinition(drv,name=nt,obj=dat,row.names=FALSE)
    
    in.tab<-paste(nt,collapse='.')
    
    #create and append add geometry field statement
    #create match table (Multi is user option)
    typematch<-data.frame(sp=c("SpatialPoints","SpatialLines","SpatialPolygons","SpatialMultiPoints"),pgis=c("Point","LineString","Polygon","Point"),
                          stringsAsFactors = FALSE)
    g.typ<-class(sdf)[1]
    
    sptype<-pmatch(typematch$sp,g.typ)
    pgtype<-na.omit(typematch$pgis[sptype==1])[1]

    if (multi) {pgtype<-paste0("Multi",pgtype)}
    if (!is.na(proj)) {pgtype<-paste0(pgtype,",",proj)}
    
    add.geom<-paste0("ALTER TABLE ",in.tab," ADD COLUMN ",geom," geometry(",pgtype,");")
    
    new.table<-paste0(new.table,"; ",add.geom)
    
  }
  
  if (!is.null(force.match)) {
    if (is.null(conn)) {stop("Database connection must be specified when using force.match.")}
    
    db.cols<-pgColumnInfo(conn,name=force.match)$column_name
    if (is.null(db.cols)) {stop(paste0("Database table ",paste(force.match,collapse='.')," not found."))}
    
    if (is.na(match(geom,db.cols))) {stop('Geometry column name not found in database table.')}
    
    rcols<-colnames(dat)
    db.cols.match<-db.cols[!is.na(match(db.cols,rcols))]
    db.cols.insert<-c(db.cols.match,geom)
    
    #reorder data frame columns
    dat<-dat[db.cols.match]
    
    message(paste0(length(colnames(dat))," out of ",length(rcols)," columns of the data frame match database table columns and will be formatted."))
    
    in.tab<-paste(force.match,collapse='.')
  } else {
    db.cols.insert<-c(colnames(dat),geom)
  }
  
  
  if (!wkb.t) { #wkt conversion
    
  message("Using writeWKT from rgeos package...")
    
  geom.1<-writeWKT(sdf,byid=TRUE)
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
    warning("spatial projection is unknown/unsupported and will be NA in insert object (SRID = 0). Use projection(sp) if you want to set it.")
    if (multi == TRUE) {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                  "',ST_Multi(ST_GeomFromText('",x[length(colnames(df))],"')))"))
    } else {
      d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                        "',ST_GeomFromText('",x[length(colnames(df))],"'))"))}
  }
  } else { #wkb conversion
    
    message("Using writeWKB from wkb package...")
    geom.1<-unlist(lapply(writeWKB(sdf),function(x) {paste(x,collapse="")}))
    df<-cbind(dat,geom.1)
    df[] <- lapply(df, as.character)
    
    #set all NA to NULL
    df[is.na(df)]<-"NULL"
    
    #double all single ' to escape
    #format rows of data frame
    if (!is.na(proj)) {
      if (multi == TRUE) {
        d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                          "',ST_Multi(ST_SetSRID('",x[length(colnames(df))],"'::geometry,",proj,")))")) 
      } else {
        d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                          "',ST_SetSRID('",x[length(colnames(df))],"'::geometry,",proj,"))"))}
    } else {
      warning("spatial projection is unknown/unsupported and will be NA in insert object (SRID = 0). Use projection(sp) if you want to set it.")
      if (multi == TRUE) {
        d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                          "',ST_Multi('",x[length(colnames(df))],"'))"))
      } else {
        d1<-apply(df,1,function(x) paste0("('",toString(paste(gsub("'","''",x[1:length(colnames(df))-1],fixed=TRUE),collapse="','")),
                                          "','",x[length(colnames(df))],"')"))}
    }
    
  }
  
  d1<-gsub("'NULL'","NULL",d1)
  d1<-paste(d1,collapse=",")
  
  lis<-list(in.table=in.tab,db.new.table=new.table,db.cols.insert=db.cols.insert,insert.data=d1)
  
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
