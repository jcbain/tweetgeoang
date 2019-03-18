#' Create Coordinates from Point Data.
#' 
#' @param data Data containing coordinates.
create_coords <- function(data){
  data %>%
    as("Spatial") %>%
    sp::coordinates()
}

#' Find Spatial Neighbors from Coordinates
#' 
#' This is a function that creates spatial neighbors from point data. It ensures
#'   that all points are linked. There are options to use either Great Distance 
#'   or Euclidean Distance as the distance function.
#'
#' @param coords Coordinate data of class "spatial".
#' @param type Distance type: "gc" for great distance.
#' @return Neighbors.
find_spatial_neighbors <- function(coords, type = "gc"){
  if( type == "gc"){
    gck1 <- spdep::knn2nb(knearneigh(coords, k=1, longlat=TRUE))
    all.linked <- max(unlist(spdep::nbdists(gck1, coords, longlat=TRUE)))
    nb <- spdep::dnearneigh(coords, 0, all.linked, longlat=TRUE)
  } else {
    llk1 <- spdep::knn2nb(knearneigh(coords, k=1, longlat=FALSE))
    all.linked <- max(unlist(spdep::nbdists(llk1, coords, longlat=FALSE)))
    nb <- spdep::dnearneigh(coords, 0, all.linked, longlat=FALSE)
  }
  nb
}