# pgInsertizeGeom
#' Formats an R sp object (Spatial* or Spatial*DataFrame) for insert (with geometry) into a PostgreSQL table (for use with pgInsert).
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
#' @param new.gid character, name of a new sequential ID column to be added to the table. 
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return List containing four character strings- a list containing four character strings- (1) in.table, the table name which will be 
#' created or inserted into, if specifed by either create.table or force.match (else NULL)
#' (2) db.new.table, the SQL statement to create the new table, if specified in create.table (else NULL), 
#' (3) db.cols.insert, a character string of the database column names to make inserts on, and 
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
#' # insert columns do not have exactly matching database table columns)
#' pgInsert(conn,c("schema","meuse_data"),pgi=pgi)
#' }

pgInsertizeGeom<- function(sdf,geom='geom',create.table=NULL,multi=FALSE,force.match=NULL,conn=NULL,new.gid=NULL) {
  
  #load wkb library if available
  wkb.t<-suppressWarnings(require("wkb",quietly = TRUE))
  
  #if spatial*dataframe, extract data frame
  dat<-data.frame()
  try(dat<-sdf@data,silent=TRUE)
  gid<-1:length(sdf)
  
  #if data frame doesn't exist, populated it with seq. id
  if(length(colnames(dat)) == 0) {
    dat<-data.frame(gid=gid)
    if (!is.null(new.gid)) {
      names(dat)[1]<-new.gid
    }
    message(paste0("No data frame; creating sequential id column (",colnames(dat),")."))
  } else { #else if it exists, add seq. id if new.gid is not null
    if (!is.null(new.gid)) {
      if (new.gid %in% colnames(dat)) {stop(paste0("'",new.gid,"' is already a column name in the data frame. Pick a unique name for new.gid or leave it null."))}
      dat<-cbind(gid,dat)
      names(dat)[1]<-new.gid
    }
  }

  rcols<-colnames(dat)
  replace <- "[+-.,!@$%^&*();/|<>]"
  in.tab<-NULL
  new.table<-NULL
  
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
    
    message("Making table names DB-compliant (replacing special characters with '_').")
    #make column names DB-compliant
    t.names<-tolower(gsub(replace,"_",rcols))
    colnames(dat)<-t.names
    
    in.tab<-paste(create.table,collapse='.')
    #make create table statement
    new.table<-postgresqlBuildTableDefinition(drv,name=in.tab,obj=dat,row.names=FALSE)
    
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
    
    db.cols<-pgColumnInfo(conn,name=force.match)$column_name
    
    if (is.na(match(geom,db.cols))) {stop('Geometry column name not found in database table.')}
    
    db.cols.match<-db.cols[!is.na(match(db.cols,rcols))]
    db.cols.insert<-c(db.cols.match,geom)
    
    #reorder data frame columns
    dat<-dat[db.cols.match]
    
    message(paste0(length(colnames(dat))," out of ",length(rcols)," columns of the data frame match database table columns and will be formatted."))
    
    in.tab<-paste(force.match,collapse='.')
  } else {
    db.cols.insert<-c(rcols,geom)
  }
  
  
  if (wkb.t) { #wkt conversion
    
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
    
    message("Using wkb package...")
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
