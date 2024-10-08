% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgInsertize.R
\name{pgInsertizeGeom}
\alias{pgInsertizeGeom}
\alias{pgInsertize}
\title{Format R data objects for insert into a PostgreSQL table.}
\usage{
pgInsertizeGeom(
  data.obj,
  geom = "geometry",
  create.table = NULL,
  force.match = NULL,
  conn = NULL,
  new.id = NULL,
  row.names = FALSE,
  alter.names = FALSE,
  partial.match = FALSE,
  df.mode = FALSE,
  geog = FALSE
)

pgInsertize(
  data.obj,
  create.table = NULL,
  force.match = NULL,
  conn = NULL,
  new.id = NULL,
  row.names = FALSE,
  alter.names = FALSE,
  partial.match = FALSE,
  df.mode = FALSE
)
}
\arguments{
\item{data.obj}{A Spatial* or Spatial*DataFrame, or data frame for
\code{pgInsertize}.}

\item{geom}{character string, the name of geometry column in the
database table. (existing or to be created; defaults to
'geom').}

\item{create.table}{character, schema and table of the PostgreSQL
table to create (actual table creation will be done in later
in pgWriteGeom().) Column names will be converted to
PostgreSQL-compliant names. Default is \code{NULL} (no new
table created).}

\item{force.match}{character, schema and table of the PostgreSQL
table to compare columns of data frame with.  If specified with
\code{partial.match = TRUE}
only columns in the data frame that exactly match the database
table will be kept, and reordered to match the database
table. If \code{NULL}, all columns will be kept in the same
order given in the data frame.}

\item{conn}{A database connection (if a table is given in for
"force.match" parameter)}

\item{new.id}{character, name of a new sequential integer ID
column to be added to the table.  (for spatial objects without
data frames, this column is created even if left \code{NULL}
and defaults to the name \code{"gid"}).}

\item{row.names}{Whether to add the data frame row names to the 
database table. Column name will be '.R_rownames'.}

\item{alter.names}{Logical, whether to make database column names
DB-compliant (remove special characters). Default is
\code{TRUE}.  (This should to be set to \code{FALSE} to match
to non-standard names in an existing database table using the
\code{force.match} setting.)}

\item{partial.match}{Logical; if force.match is set and  true, 
columns in R data frame will be compared with an the 
existing database table \code{name}. Only columns in the 
data frame that exactly match the database
table will be inserted into the database table.}

\item{df.mode}{Logical; Whether to write data in data frame mode 
(preserving data frame column attributes and row.names).
A new table must be created with this mode (or overwrite set to TRUE),
and the row.names, alter.names, and new.id arguments will
be ignored (see \code{dbWriteDataFrame} for more information.}

\item{geog}{Logical; Whether to write the spatial data as a PostGIS 
'GEOGRAPHY' type.}
}
\value{
pgi A list containing four character strings: (1)
    in.table, the table name which will be created or inserted
    into, if specified by either create.table or force.match (else
    NULL) (2) db.new.table, the SQL statement to create the new
    table, if specified in create.table (else NULL), (3)
    db.cols.insert, a character string of the database column
    names to insert into, and (4) insert.data, a character string
    of the data to insert. See examples for usage within the
    \code{pgInsert} function.
}
\description{
These are internal rpostgis functions that take an R \code{sp} or \code{sf} object (Spatial*,
Spatial*DataFrame or sf; for \code{pgInsertizeGeom}), or data frame (for
\code{pgInsertize}) and return a \code{pgi} list object, which can
be used in the function \code{pgInsert} to insert rows of the
object into the database table. (Note that these functions do not
do any modification of the database, it only prepares the data for
insert.) The function \code{pgInsert} is a wrapper around these
functions, so \code{pgInsertize*} should only be used in
situations where data preparation and insert need to be separated.
}
\details{
The entire data frame is prepared by default, unless
\code{force.match} specifies a database table (along with a
database connection \code{conn}), in which case the R column names
are compared to the \code{force.match} column names, and only
exact matches are formatted to be inserted.

A new database table can also be prepared to be created using the
\code{create.table} argument.  If \code{new.id} is specified, a
new sequential integer field is added to the data frame.  For
\code{Spatial*}-only objects (no data frame), a new.id is created
by default with name \code{gid}.  For \code{pgInsertizeGeom}, if
the R package \code{wkb} is installed, this function uses
\code{writeWKB} to translate the geometries for some spatial types
(faster with large datasets), otherwise the \code{rgeos} function
\code{writeWKT} is used.
}
\examples{
\dontrun{
library(sp)
data(meuse)
coords <- SpatialPoints(meuse[, c("x", "y")])
spdf <- SpatialPointsDataFrame(coords, meuse)

## Format data for insert
pgi.new <- pgInsertizeGeom(spdf, geom = "point_geom", create.table = c("schema",
    "table"), new.id = "pt_gid")
print(pgi.new)

## Insert data in database table (note that an error will be given if
## all insert columns do not have exactly matching database table
## columns)
pgWriteGeom(conn = conn, data.obj = pgi.new)

## Inserting into existing table
pgi.existing <- pgInsertizeGeom(spdf, geom = "point_geom", force.match = c("schema",
    "table"), conn = conn)
## A warning message is given, since the "dist.m" column is not found
## in the database table (it was changed to "dist_m" in pgi.new to
## make name DB-compliant). All other columns are prepared for insert.
print(pgi.existing)

pgWriteGeom(conn = conn, data.obj = pgi.existing)
}
\dontrun{
## Format regular (non-spatial) data frame for insert using
## pgInsertize connect to database
data <- data.frame(a = c(1, 2, 3), b = c(4, NA, 6), c = c(7,
    "text", 9))

## Format non-spatial data frame for insert
values <- pgInsertize(data.obj = data)

## Insert data in database table (note that an error will be given if
## all insert columns do not match exactly to database table columns)
pgWriteGeom(conn, data.obj = values, name = c("schema", "table"))

## Run with forced matching of database table column names
values <- pgInsertize(data.obj = data, force.match = c("schema",
    "table"), conn = conn)

pgWriteGeom(conn, data.obj = values)
}
}
\author{
David Bucklin \email{david.bucklin@gmail.com}
}
\keyword{internal}
