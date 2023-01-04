#'LandComp: quantify landscape diversity and structure
#'
#'Calculate compositional diversity and associatum of landscape data at
#'different spatial scales.
#'
#'@param x An \code{sf} object of type \code{"POLYGON"} that must have projected
#'  coordinates (i.e. WGS-84 is not accepted). Geometry must be a regular
#'  spatial grid containing either squares or hexagons. Both flat topped and
#'  pointy topped hexagons arerotated_geometry_of_the_selected_gridcell accepted. Fields should contain binary integer
#'  values (i.e., 0s and 1s). Logical values are coerced with warning.
#'@param aggregation_steps A numeric vector containing non-negative numbers. The
#'  vector elements express the size of the spatial units for which calculation
#'  of compositional diversity and associatum is required. The size is measured
#'  by the number of rows of grid cells around the central grid cell, where 0
#'  means the original grid cell without enlargement. Analysis can be done more
#'  precise by giving also fraction numbers as input. In this case the following
#'  step's rosette minus its vertices are used as spatial base units. Note, in
#'  the case of hexagonal grid, steps falling in the interval ]0,1[ cannot be
#'  evaluated. Negative, non-finite and missing values are ignored with warning.
#'@param parallelrun A logical vector of length one indicating whether
#'  aggregation should be performed in a parallel way (defaults to \code{TRUE}).
#'  All available processor cores are used in the case of parallel processing.
#'  Should be set to \code{FALSE} if memory limitation occurs.
#'@param savememory A logical vector of length one indicating whether a slower
#'  but less memory-demanding algorithm should run (defaults to \code{FALSE}).
#'  Should be set to \code{TRUE} if the available memory is limited.
#'@param precision A numeric vector of length one. Number of digits to which the
#'  areas of grid cells are rounded. Should be decreased if the grid is not
#'  perfectly regular and the equality check of the grid cells' area fails.

#'
#'@return A \code{data.frame} of \code{length(aggregation_steps)} rows with the
#'  following columns and attribute:
#' * **AggregationStep**: size of the spatial units measured by
#'  the number of rows of grid cells around the central grid cell. The content
#'  (and order) of this column is the same as the parameter
#'  \code{aggregation_steps} except that negative, non-finite and missing values
#'  are removed. It is a sort of ID in the resulted \code{data.frame}.
#' * **SpatialUnit_Size**: number of grid cells contained by the aggregated,
#' large unit.
#' * **SpatialUnit_Area**: area of the aggregated, large unit
#' * **SpatialUnit_Count**: sample size.
#' * **UniqueCombination_Count**: number of unique landscape class combinations.
#' * **CD_bit**: compositional diversity (sensu Juhász-Nagy) of \code{x}.
#' * **AS_bit**: associatum (sensu Juhász-Nagy) of  \code{x}
#' * **attr(*, "unit")**: unit of the CRS of the object provided to \code{x}.
#'
#'@export
#'@keywords landscape diversity, multilayer analysis, Juhász-Nagy's functions
#'
#'@examples
#' data(square_data)
#' LandComp(x = square_data, aggregation_steps = 0, parallelrun = FALSE)
#' LandComp(x = square_data, aggregation_steps = c(0.5, 1, 1.5))
#'
#' data(hexagonal_data)
#' LandComp(x = hexagonal_data, aggregation_steps = c(0, 1, 1.5))
#'
#'@references
#' * Juhász-Nagy P (1976) Spatial dependence of plant populations. Part 1.
#'Equivalence analysis (An outline of new model). Acta Bot Acad Sci Hung
#'22:61–78.
#' * Juhász-Nagy P (1984) Spatial dependence of plant population. 2. A family
#'of new models. Acta Bot Hung 30:363–402.
#' * Juhász-Nagy P (1993) Notes on compositional diversity. Hydrobiologia
#'249:173–182.
#' * Juhász-Nagy P, Podani J (1983) Information theory methods for the study of
#'spatial processes and succession. Vegetatio 51:129–140.
#'
LandComp <- function(x, aggregation_steps = c(0, 1, 1.5, 2:5), parallelrun = TRUE, savememory = FALSE, precision = 4){
  # Checking required packages
  sf_available <- requireNamespace(package = "sf", quietly = TRUE)
  if(!sf_available) stop("Package 'sf' is not available. Please install it.")
  future_available <- requireNamespace(package = "future", quietly = TRUE)
  if(!future_available) stop("Package 'future' is not available. Please install it.")
  futureapply_available <- requireNamespace(package = "future.apply", quietly = TRUE)
  if(!futureapply_available) stop("Package 'future.apply' is not available. Please install it.")

  # Checking parameters
  if(missing(x)) stop("Parameter 'x' should be set.")
  if(missing(aggregation_steps)) stop("Parameter 'aggregation_step' should be set.")
  if(!("sf" %in% class(x))) stop("Parameter 'x' should have class 'sf'.")
  sf::st_agr(x) <- "constant"
  rownames(x) <- paste0("id", 1:nrow(x))
  geometry <- sf::st_geometry(x)
  x <- sf::st_drop_geometry(x)
  all_geometries_are_polygon <- all(sf::st_is(x = geometry, type = "POLYGON"))
  if(!all_geometries_are_polygon) stop("Parameter 'x' should contain only data of polygons.")
  if(is.na(sf::st_crs(geometry))){ stop("CRS of parameter 'x' should be set but found to be missing.")
  }else if(sf::st_is_longlat(geometry)) stop("Parameter 'x' should have projected coordinates. Resampling and application of another CRS is suggested.")
  if(!all(sort(unique(unlist(x))) %in% c(0,1)) ) stop(paste0("Parameter 'x' should contain binary data, with 0 (absence) and 1 (presence) data. However, other values are found: ", paste0(utils::head(sort(unique(unlist(x))), 10), collapse = ", ")))
  if(is.logical(x)){
    x <- as.integer(x)
    warning("Parameter 'x' was found holding a logical data though integer is required. Coercion is done.")
  }
  if(length(aggregation_steps)<1) stop("Parameter 'aggregation_steps' should be set.")
  if(any(is.na(aggregation_steps))){
    aggregation_steps <- aggregation_steps[!is.na(aggregation_steps)]
    warning("Unknown (NA) values provided to parameter 'aggregation_steps' are ignored.")
  }
  if(!is.numeric(aggregation_steps)) stop("Parameter 'aggregation_steps' should be numeric.")
  if(any(aggregation_steps < 0 | !is.finite(aggregation_steps))){
    mask <- aggregation_steps >= 0 & is.finite(aggregation_steps)
    warning(paste0("Parameter 'aggregation_steps' requires finite, non-negative values. Others (", paste0(utils::head(aggregation_steps[!mask], 10), collapse = ", "), ") are ignored."))
    aggregation_steps <- aggregation_steps[mask]
  }
  if(length(parallelrun)<1) {
    stop("Parameter 'parallelrun' should be of length one.")
  } else if(length(parallelrun)>1){
    warning(paste0("Parameter 'parallelrun' has more elements (", as.character(length(parallelrun)), ") then expected (1). Only the first element is used."))
    parallelrun <- parallelrun[1]
  }
  if(!is.logical(parallelrun)) stop("Parameter 'parallelrun' should be logical.")
  if(is.na(parallelrun)) stop("Unknown (NA) values provided to parameter 'parallelrun'.")
  if(length(savememory)<1) {
    stop("Parameter 'savememory' should be of length one.")
  } else if( length(savememory)>1){
    warning(paste0("Parameter 'savememory' has more elements (", as.character(length(savememory)), ") then expected (1). Only the first element is used."))
    savememory <- savememory[1]
  }
  if(!is.logical(savememory)) stop("Parameter 'savememory' should be logical.")
  if(is.na(savememory)) stop("Unknown (NA) values provided to parameter 'savememory'.")
  if(length(precision)<1) {
    stop("Parameter 'precision' should be of length one.")
  } else if(length(precision)>1){
    warning(paste0("Parameter 'precision' has more elements (", as.character(length(precision)), ") then expected (1). Only the first element is used."))
    precision <- precision[1]
  }
  if(!is.numeric(precision)) stop("Parameter 'precision' should be numeric.")
  if(any(is.na(precision))) stop("Unknown (NA) values provided to parameter 'precision'.")
  if(any(precision <= 0 | as.logical(precision %% 1) | !is.finite(precision))) stop(paste0("Parameter 'precision' requires positive integers. Inappropriate value (", precision, ") is detected."))
  # Settings related to future package
  system_name <- base::Sys.info()["sysname"]
  default_strategy_of_plan <- if(parallelrun) {
    if(grepl(pattern = "Linux", x = system_name)){
      future::plan("future::multicore")
    }else{
      future::plan("future::multisession")
    }
  } else {
    future::plan("future::sequential")
  }
  default_maxGlobalSize <- options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
  number_of_vertices <- unlist(future.apply::future_lapply(X = 1:length(geometry), future.seed = NULL, FUN = function(i){length(sf::st_cast(sf::st_boundary(geometry[i]), "POINT")[-1])}))
  if(all(number_of_vertices == 4)) {
    square = TRUE
  } else if(all(number_of_vertices == 6)){
    square = FALSE
  } else if(length(unique(vertices))>1){
    stop(paste0("The geometry of 'x' seems to be not regular. Different number of vertices are detected: ", paste0(utils::head(unique(vertices), n = 10), collapse = ", ")))
  } else stop(paste0("The geometry of 'x' should contain either regular square grid or regular hexagonal grid. However, other number of vertices is detected: ", unique(vertices), "."))
  if(any(aggregation_steps > 0 & aggregation_steps < 1 & !square )){
    mask <- !(aggregation_steps > 0 & aggregation_steps < 1 & !square )
    warning(paste0("Values falling in the range ]0,1[ set to parameter 'aggregation_steps' cannot be evaluated when using hexagonal grid. These (", paste0(utils::head(aggregation_steps[!mask], n = 10), collapse = ", "), ") are ignored."))
    aggregation_steps <- aggregation_steps[mask]
  }
  if(length(aggregation_steps) < 1) stop("All the values provided to parameter 'aggregation_steps' were found to be inappropriate and thus were ignored. Please provide appropriate values.")

  # Preparation for calculation
  selected_gridcell <- 1
  gridcell_area <- unique(round(sf::st_area(geometry), digits = precision))
  centroid <- sf::st_centroid(geometry)
  if(length(gridcell_area)>1) stop(paste0("The grid provided to parameter 'x' seems to be not regular. Grid cells with different areas ", paste0(utils::head(gridcell_area, 10), collapse = ", "), " have been found."))
  if(square){
    side_length <- diff(sf::st_bbox(geometry[1])[c(1,3)])
  }else{
    smaller_diameter <- min(c(diff(sf::st_bbox(geometry[1])[c(1,3)]), diff(sf::st_bbox(geometry[1])[c(2,4)])))
    side_length <- smaller_diameter / sqrt(3)
    rotate = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    rotated_geometry_of_the_selected_gridcell <- (geometry[selected_gridcell] - centroid) * rotate(pi/2) + centroid
    sf::st_crs(rotated_geometry_of_the_selected_gridcell) <- sf::st_crs(geometry)
  }

  # Initializing the data frame for result statistics
  stats <- c("AggregationStep",
             "SpatialUnit_Size",              # size of the spatial unit of the given aggregation step
             "SpatialUnit_Area",              # area of the spatial unit of the given aggregation step
             "SpatialUnit_Count",             # number of the spatial units of the given aggregation step
             "UniqueCombination_Count",       # number of unique combinations
             "CD_bit",                        # Juhasz-Nagy's function: compositional diversity
             "AS_bit")                        # Juhasz-Nagy's function: associatum
  results <- as.data.frame(
               matrix(rep(NA),
                 nrow = length(aggregation_steps),
                 ncol = length(stats),
                 dimnames = list(NULL, stats)))
  results[,"AggregationStep"] <- aggregation_steps

  # Creating aggregated data
  sorted_aggregation_steps <- sort(unique(aggregation_steps), decreasing = TRUE)
  for(aggregation_step in sorted_aggregation_steps){
    SpatialUnit_Size <- calculate_SpatialUnitSize(aggregation_step = aggregation_step, square = square)
    if(aggregation_step == 0){
      aggregated_data <- x
    }else{
      # Creating buffer
      aggregation_step_is_integer <- (aggregation_step %% 1) == 0
      if(square){
        distance <- ifelse(aggregation_step_is_integer,
                           side_length * (floor(aggregation_step)+0.0001) ,
                           side_length * (floor(aggregation_step)+0.5001) )
        buffer_of_the_selected_gridcell <- sf::st_buffer(geometry[selected_gridcell], dist = distance, joinStyle = 'MITRE', mitreLimit = 10)
      }else{
        distance <- ifelse(aggregation_step_is_integer,
                           1.5 * side_length * floor(aggregation_step) + 0.0001 ,
                           1.5 * side_length * (floor(aggregation_step) + 0.5001) )
        buffer_of_the_selected_gridcell <- sf::st_buffer(rotated_geometry_of_the_selected_gridcell, dist = distance, joinStyle = "MITRE", mitreLimit = 10)
      } #if - whether square or hexagon is the grid cell
      buffer <- buffer_of_the_selected_gridcell + (centroid - sf::st_centroid(buffer_of_the_selected_gridcell))
      sf::st_crs(buffer) <- sf::st_crs(geometry)

      # Defining grid cell IDs covered by the buffer
      if(aggregation_step == sorted_aggregation_steps[1] | savememory){
        st_par <- function(X, FUN, ...){
          n_workers <- future::nbrOfWorkers()
          if ("sf" %in% class(X)) {
            n_elements <- nrow(X)
          } else if ("sfc" %in% class(X)) {
            n_elements <- length(X)
          } else {
            stop("Unknown input class. st_par() only accepts the following inputs: sfc, sf.")
          }
          fold_length <- floor(n_elements / n_workers)
          split_vector <- rep(x = 1:n_workers, times = c(rep(x = fold_length, times = n_workers - 1), n_elements - fold_length * (n_workers - 1)))
          split_results <- future.apply::future_lapply(X = split(X, split_vector), function(x) FUN(x, ...))
          output_class <- class(split_results[[1]])
          if (length(output_class) == 2) output_class <- output_class[2]
          if (output_class == "sfc") {
            result <- do.call("c", split_results)
            result <- FUN(result)
          } else if (output_class %in% c('list', 'sgbp') ){
            result <- do.call("c", split_results)
            names(result) <- NULL
          } else {
            stop("Unknown output class. st_par() only accepts the following outputs: sfc, list, sf, sgbp.")
          }
          return(result)
        } # end of st_par()

        covered_gridcellIDs <- st_par(X = buffer, FUN = sf::st_contains, y = centroid)
      }else{ # When parallelization is not applied, to make faster the running, covered gridcellIDs are selected from that of the previous step.
	      covered_gridcellIDs <- future.apply::future_lapply(X = 1: length(covered_gridcellIDs), future.seed = NULL, FUN = function(i) {
	       ith_covered_gridcellIDs <- covered_gridcellIDs[[i]]
	       ith_mask <- sf::st_contains(buffer[i], centroid[ith_covered_gridcellIDs])[[1]]
	       ith_covered_gridcellIDs[ith_mask]})
      }
      if(!aggregation_step_is_integer){ # if aggregation step is fraction, corners of the moving window are deleted
	      vertices <- future.apply::future_lapply(X = 1:length(buffer), future.seed = NULL, FUN = function(i){sf::st_cast(sf::st_boundary(buffer[i]), "POINT")[-1]})
        covered_gridcellIDs <- future.apply::future_lapply(X = 1:length(covered_gridcellIDs), future.seed = NULL, FUN = function(i){
          indices_of_gridcells_to_delete = unlist(sf::st_within(vertices[[i]], geometry[covered_gridcellIDs[[i]]]))
          covered_gridcellIDs[[i]][setdiff(x = 1:length(covered_gridcellIDs[[i]]), y = indices_of_gridcells_to_delete)]
        })
      }

      # Selecting spatial units filled in entirely with data of grid cells
      names(covered_gridcellIDs) <- rownames(x)
      mask = vapply(X = covered_gridcellIDs, FUN.VALUE = logical(length = 1),
        FUN = function(element){length(element) == SpatialUnit_Size}) # large spatial units with gaps of data should be not taken into account when performing calculation of information theory functions
      entirely_covered_gridcellIDs = covered_gridcellIDs[mask]

      # Aggregating by maximum
      aggregated_data_list <- future.apply::future_lapply(X = entirely_covered_gridcellIDs, future.seed = NULL, FUN = function(gridcells_to_aggregate){
				 if(length(x)>1){ apply(x[gridcells_to_aggregate,], MARGIN = 2, FUN = max) # KKD modositotta 2023 01 04
         }else{ max(x[gridcells_to_aggregate,])}# KKD modositotta 2023 01 04
      })

      aggregated_data <- as.data.frame(do.call(what = rbind, args = aggregated_data_list))
      rm(list = c("aggregated_data_list", "buffer", "entirely_covered_gridcellIDs", "mask"))
    } # if - whether aggregation is needed

    # Calculation
    # Calculating measures needed for calculation of information theory functions
    mask_of_rows <- (aggregation_step == results$AggregationStep)
    gridcell_area <- sf::st_area(geometry[1])
    SpatialUnit_Count <- nrow(aggregated_data)
    results[mask_of_rows, "SpatialUnit_Count"] <- SpatialUnit_Count
    results[mask_of_rows, "SpatialUnit_Size"] <- SpatialUnit_Size
    results[mask_of_rows, "SpatialUnit_Area"] <- SpatialUnit_Size * gridcell_area

    # Calculating compositional diversity
    combinations <- apply(aggregated_data, MARGIN = 1, FUN = paste0, collapse="")
    p_combinations <- as.vector(table(combinations)/SpatialUnit_Count) # proportion of the unique landscape class combinations
    CD <- - sum(p_combinations * log2(p_combinations))
    results[mask_of_rows, "CD_bit"] <- CD
    results[mask_of_rows, "UniqueCombination_Count"] <- length(unique(combinations))

    # Calculating associatum
    preference_of_landscape_classes <- apply(aggregated_data, MARGIN=2, FUN=function(landscape_class){ # preference_of_landscape_classes: counts of presences (1) and absences (0) of each landscape classes
       factorized_landscape_class = factor(landscape_class, levels=c(0,1)) # if a class is present/absent in all the spatial units, then factorization facilitates handling the result of table()
       table(factorized_landscape_class)/SpatialUnit_Count})
    preference_of_landscape_classes <- as.vector(preference_of_landscape_classes)  # Technically a vector is needed for calculation.
    LD <- -sum(preference_of_landscape_classes * log2(preference_of_landscape_classes), na.rm=TRUE) # local distinctiveness
    AS <- LD - CD
    results[mask_of_rows, "AS_bit"] <- AS

  }# for - aggregation step
  attr(x = results, which = "unit") <- sf::st_crs(geometry)$units_gdal
  future::plan(default_strategy_of_plan)
  options(future.globals.maxSize = default_maxGlobalSize)
  return(results)
}
