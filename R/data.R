#' Example data for GPS tracking project
#'
#' Example datasets related to a GPS tracking project for roe deer
#' in Trentino Region, Italy. Four datasets include raw data from GPS sensors (\code{db_gps_data}),
#' information on animals, sensors, and  sensor deployments on animals (\code{db_sensors_animals_tables}),
#' and ancilliary vector (\code{db_vector_geom}) and raster (\code{db_raster})
#' spatial datasets.
#'
#' @format \code{db_gps_data}: A list containing five \code{data.frame}s corresponding to five GPS
#' sensors
#'  \describe{
#'    \item{GSM01438}{data frame for sensor 01438}
#'    \item{GSM01508}{data frame for sensor 01508}
#'    \item{GSM01511}{data frame for sensor 01511}
#'    \item{GSM01512}{data frame for sensor 01512}
#'    \item{GSM02927}{data frame for sensor 02927}
#'  }
#' 
#' \code{db_sensors_animals_tables}: A list containing three \code{data.frame}s
#'  \describe{
#'    \item{animals}{data frame containing basic information on animals}
#'    \item{gps_sensors}{data frame containing basic information on GPS sensors}
#'    \item{gps_sensors_animals}{data frame containing information on deployment of GPS sensors on animals}
#'  }
#'
#' \code{db_vector_geom}: A list containing four \code{Spatial*DataFrame}s
#'  \describe{
#'    \item{study_area}{SpatialPolygonsDataFrame containing boundary of study area}
#'    \item{adm_boundaries}{SpatialPolygonsDataFrame containing administrative boundaries in study area}
#'    \item{meteo_stations}{SpatialPointsDataFrame containing locations of weather stations in study area}
#'    \item{roads}{SpatialLinesDataFrame containing representation of roads for study area}
#'  }
#' 
#' \code{db_raster}: A list containing two \code{RasterLayer} datasets
#'  \describe{
#'    \item{corine06}{RasterLayer depicting land cover classification in the study area}
#'    \item{srtm_dem}{RasterLayer digital elevation model in the study area}
#'  }
#'  
#' @source Urbano, Ferdinando, and Francesca Cagnacci, eds.
#' \emph{Spatial database for GPS wildlife tracking data: a practical guide to creating a data management system with PostgreSQL/PostGIS and R.}
#' Cham, Switzerland: Springer, 2014. \url{http://www.springer.com/us/book/9783319037424}.
#' 
#' @examples
#'  data("db_gps_data")
#'  head(db_gps_data$GSM01438)
#' 
"db_gps_data"

#' @rdname db_gps_data
#' @examples 
#'  data("db_sensors_animals_tables")
#'  db_sensors_animals_tables$animals
#' 
"db_sensors_animals_tables"

#' @rdname db_gps_data
#' @examples
#'  data("db_vector_geom")
#'  if (require(sp, quietly = TRUE)) {
#'  plot(db_vector_geom$adm_boundaries)
#'  plot(db_vector_geom$roads, col = 'red', add = TRUE)
#'  }
#' 
"db_vector_geom"

#' @rdname db_gps_data
#' @examples
#'  data("db_raster")
#'  if (require(raster, quietly = TRUE)) plot(db_raster$srtm_dem)
#' 
"db_raster"