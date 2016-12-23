#' GPS tracking core data
#'
#' Dataset containing a set of five raw GPS data tables from sensors
#' attached to roe deer in Trentino Region, Italy
#'
#' @format A list containing five data frames corresponding to five GPS
#' sensors
#' \describe{
#'   \item{GSM01438}{data frame for sensor 01438}
#'   \item{GSM01508}{data frame for sensor 01508}
#'   \item{GSM01511}{data frame for sensor 01511}
#'   \item{GSM01512}{data frame for sensor 01512}
#'   \item{GSM02927}{data frame for sensor 02927}
#' }
#' @source Urbano, Ferdinando, and Francesca Cagnacci, eds.
#' \emph{Spatial database for GPS wildlife tracking data: a practical guide to creating a data management system with PostgreSQL/PostGIS and R.}
#' Cham, Switzerland: Springer, 2014. \url{http://www.springer.com/us/book/9783319037424}.
#' 
#' @examples
#'  data("db_gps_data")
#'  head(db_gps_data$GSM01438)
#' 
"db_gps_data"


#' GPS sensor and animal metadata
#'
#' Dataset containing information on individual animals, sensors,
#' and sensors deployment on roe deer in Trentino Region, Italy
#'
#' @format A list containing three data frames
#' \describe{
#'   \item{animals}{data frame containing basic information on animals}
#'   \item{gps_sensors}{data frame containing basic information on GPS sensors}
#'   \item{gps_sensors_animals}{data frame containing information on deployment of GPS sensors on animals}
#' }
#' @source Urbano, Ferdinando, and Francesca Cagnacci, eds.
#' \emph{Spatial database for GPS wildlife tracking data: a practical guide to creating a data management system with PostgreSQL/PostGIS and R.}
#' Cham, Switzerland: Springer, 2014. \url{http://www.springer.com/us/book/9783319037424}.
#' 
#' @examples 
#'  data("db_sensors_animals_tables")
#'  db_sensors_animals_tables$animals
#' 
"db_sensors_animals_tables"


#' Vector spatial datasets
#'
#' Dataset containg ancilliary vector spatial data related to GPS tracking
#' of roe deer in Trentino Region, Italy
#'
#' @format A list containing four Spatial*DataFrames
#' \describe{
#'   \item{study_area}{SpatialPolygonsDataFrame containing boundary of study area}
#'   \item{adm_boundaries}{SpatialPolygonsDataFrame containing administrative boundaries in study area}
#'   \item{meteo_stations}{SpatialPointsDataFrame containing locations of weather stations in study area}
#'   \item{roads}{SpatialLinesDataFrame containing representation of roads for study area}
#' }
#' @source Urbano, Ferdinando, and Francesca Cagnacci, eds.
#' \emph{Spatial database for GPS wildlife tracking data: a practical guide to creating a data management system with PostgreSQL/PostGIS and R.}
#' Cham, Switzerland: Springer, 2014. \url{http://www.springer.com/us/book/9783319037424}.
#'
#' @examples
#'  data("db_vector_geom")
#'  if (require(sp, quietly = TRUE)) {
#'  plot(db_vector_geom$adm_boundaries)
#'  plot(db_vector_geom$roads, col = 'red', add = TRUE)
#'  }
#' 
"db_vector_geom"


#' Raster spatial datasets
#'
#' Dataset containin ancilliary raster spatial data related to GPS tracking 
#' of roe deer in Trentino Region, Italy
#'
#' @format A list containing two RasterLayer datasets
#' \describe{
#'   \item{corine06}{RasterLayer depicting land cover classification in the study area}
#'   \item{srtm_dem}{RasterLayer digital elevation model in the study area}
#' }
#' @source Urbano, Ferdinando, and Francesca Cagnacci, eds.
#' \emph{Spatial database for GPS wildlife tracking data: a practical guide to creating a data management system with PostgreSQL/PostGIS and R.}
#' Cham, Switzerland: Springer, 2014. \url{http://www.springer.com/us/book/9783319037424}.
#' 
#' @examples
#'  data("db_raster")
#'  if (require(raster, quietly = TRUE)) plot(db_raster$srtm_dem)
#' 
"db_raster"