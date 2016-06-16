library(RPostgreSQL)
library(adehabitatLT)
library(adehabitatHS)

#to install development version of rpostgis, clone the repo to your computer and use devtools to install it:
library(devtools)
setwd('/git/rpostgis/')
install()

library(rpostgis)

#connect to database
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="", password="", dbname="",host="basille-flrec.ad.ufl.edu")

# get sample ltraj
data(ibex)
ibex_df <- ld(ibex) # convert to data frame

dbSendQuery(conn,"drop table ibex1;")
dbSendQuery(conn,"drop table ibex2;")

# first create table
tab<-postgresqlBuildTableDefinition(drv,name=c('ibex1'),obj=ibex_df,row.names=FALSE)
# Note that "." can be used in column names in PostgreSQL,
# but then column names are always enclosed in double quoatations("col_name").
# I worked around this by always enclosing all column names in 
# insert statements from R in double parentheses (in pgInsert() function)
dbSendQuery(conn, tab)


#TESTS

#test1 (create new table with PostgreSQL compliant names)
# only when create.table is used, are column names altered in the pgi object
records <- pgInsertize(df=ibex_df,create.table="ibex2")
print(records)
pgInsert(conn,pgi=records,name='ibex2')


#test2 (insert into existing table (not specified until insert), no checks on column names)
records <- pgInsertize(df=ibex_df)
print(records)

#what happens when column names do not match
pgInsert(conn,pgi=records,name='ibex2')

#send to 'ibex1', with perfectly matching column names
pgInsert(conn,pgi=records,name='ibex1')


#test3 (work on existing table)
#use force.match, will only format matching column names
#this test has partial matching
records <- pgInsertize(df=ibex_df,con=conn,force.match=c("ibex2"))
records
pgInsert(conn,pgi=records)

#user should modify R column names prior to match existing DB columns, using info from pgColumnInfo
cols<-pgColumnInfo(conn,"ibex1")
colnames(ibex_df)<-cols$column_name

# since we manually matched all column names above, force.match indicates all columns are formatted for insert
records <- pgInsertize(df=ibex_df,con=conn,force.match=c("ibex1"))
records
pgInsert(conn,pgi=records)

# when table was used in pgInsertize, table name should not be specified in pgInsert() 
# name could be simply ignored, though I think it is safer to throw an error as below
pgInsert(conn,records,name='ibex2')


#drop tables
dbSendQuery(conn,"drop table ibex1;")
dbSendQuery(conn,"drop table ibex2;")


#TODO

# 1. update pgInsertizeGeom functionality to match pgInsertize

# 2. test wkb functionality for pgInsertizeGeom (done; WKB is much faster for large sp objects (>10000 lines))
# I suspect this could enhance processing speed simply be reducing length of geometry strings (vs. WKT)
# For large tables pgInsertizeGeom can be pretty slow (with writeWKT)

# 3. turn of printing of PostgreSQL log messages for acutal insert - messages get very long.
# I'm not sure at which level this is controlled, maybe it is dependent on PostgreSQL settings. 
# R's suppressMessages and suppressWarnings were not working.