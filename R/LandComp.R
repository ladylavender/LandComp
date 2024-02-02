#'LandComp: quantify landscape diversity and structure
#'
#'Calculate compositional diversity and associatum of landscape data at
#'different spatial scales.
#'
#'@encoding UTF-8
#'@param x An `sf` object of type `POLYGON` that must have projected coordinates
#'  (i.e. WGS-84 is not accepted). Geometry must be a regular spatial grid
#'  containing either squares or hexagons. Both flat topped and pointy topped
#'  hexagons are accepted. Fields should contain binary integer values (i.e., 0s
#'  and 1s). Logical values are coerced with warning.
#'@param aggregation_steps A numeric vector containing non-negative numbers. The
#'  vector elements express the size of the spatial units for which calculation
#'  of compositional diversity and associatum is required. The size is measured
#'  by the number of rows of grid cells around the central grid cell, where 0
#'  means the original grid cell without enlargement. Analysis can be done more
#'  precise by giving also fraction numbers as input. In this case, the
#'  following step's spatial unit minus grid cells touching the vertices are
#'  used as spatial base units. Note, in the case of hexagonal grid, steps
#'  falling in the interval ]0,1[ cannot be evaluated. Negative, non-finite and
#'  missing values are ignored with warning.
#'@param parallelrun A logical vector of length one indicating whether
#'  aggregation should be performed in a parallel way (defaults to `TRUE`). All
#'  available processor cores are used in the case of parallel processing.
#'  Should be set to `FALSE` if memory limitation occurs.
#'@param savememory A logical vector of length one indicating whether a slower
#'  but less memory-demanding algorithm should run (defaults to `FALSE`). Should
#'  be set to `TRUE` if the available memory is limited.
#'@param precision A numeric vector of length one. Number of digits to which the
#'  areas of grid cells are rounded. Should be decreased if the grid is not
#'  perfectly regular and the equality check of the grid cells' area fails.

#'
#'@return A `data.frame` of `length(aggregation_steps)` rows with the following
#'  columns and attribute:
#' * **AggregationStep**: size of the spatial units measured by
#'  the number of rows of grid cells around the central grid cell. The content
#'  (and order) of this column is the same as the parameter
#'  \code{aggregation_steps} except that negative, non-finite and missing values
#'  are removed. It also serves as an ID in the resulting `data.frame`.
#' * **SpatialUnit_Size**: number of grid cells contained by the aggregated,
#'  large unit.
#' * **SpatialUnit_Area**: area of the aggregated, large unit
#' * **SpatialUnit_Count**: sample size.
#' * **UniqueCombination_Count**: number of unique landscape class combinations.
#' * **CD_bit**: compositional diversity (sensu Juhász-Nagy) of `x`.
#' * **AS_bit**: associatum (sensu Juhász-Nagy) of  `x`
#' * **attr(*, "unit")**: unit of the CRS of the object provided to `x`.
#'
#'@details The function is based on the model family created by Juhász-Nagy
#'  (1976, 1984, 1993). Compositional diversity
#'  (\ifelse{html}{\out{CD}}{\eqn{CD}}) measures the diversity of landscape
#'  class combinations. Associatum (\ifelse{html}{\out{AS}}{\eqn{AS}})
#'  characterizes the spatial dependence of landscape classes. It is measured as
#'  the difference of the "random" diversity (i.e. predicted diversity with the
#'  assumption of independent occurrence of landscape classes) and the observed
#'  diversity. Both functions have typically one maximum
#'  (\ifelse{html}{\out{CD<sub>max</sub>}}{\eqn{CD_{max}}},
#'  \ifelse{html}{\out{AS<sub>max</sub>}}{\eqn{AS_{max}}}), when plotting
#'  against increasing scale. Unit sizes corresponding to the maxima values of
#'  both functions (\ifelse{html}{\out{A<sub>CD</sub>}}{\eqn{A_{CD}}},
#'  \ifelse{html}{\out{A<sub>CD</sub>}}{\eqn{A_{AS}}}) help to capture the
#'  spatial scale holding the most information. These indices, particularly
#'  \ifelse{html}{\out{CD<sub>max</sub>}}{\eqn{CD_{max}}},
#'  \ifelse{html}{\out{AS<sub>max</sub>}}{\eqn{AS_{max}}} and
#'  \ifelse{html}{\out{A<sub>CD</sub>}}{\eqn{A_{CD}}} can be effectively used as
#'  indicators (Juhász-Nagy & Podani 1983). Though the functions were originally
#'  applied in community ecology, the current function supports their
#'  application in the landscape context (see also Konrád et al. 2023).
#'
#'@concept landscape diversity
#'@concept multilayer analysis
#'@concept Juhász-Nagy's functions
#'
#'@examples
#' data(square_data)
#' LandComp(x = square_data, aggregation_steps = 0)
#'
#'\donttest{
#' LandComp(x = square_data, aggregation_steps = 0, parallelrun = FALSE)
#' LandComp(x = square_data, aggregation_steps = c(0.5, 1, 1.5))
#'
#' data(hexagonal_data)
#' LandComp(x = hexagonal_data, aggregation_steps = c(0, 1, 1.5))
#'}
#'
#'@references
#' * Juhász-Nagy P (1976) Spatial dependence of plant populations. Part 1.
#'Equivalence analysis (An outline of new model). _Acta Bot Acad Sci Hung 22_:
#'61–78.
#' * Juhász-Nagy P (1984) Spatial dependence of plant population. 2. A family
#'of new models. _Acta Bot Hung 30_: 363–402.
#' * Juhász-Nagy P (1993) Notes on compositional diversity. _Hydrobiologia 249_:
#'173–182.
#' * Juhász-Nagy P, Podani J (1983) Information theory methods for the study of
#'spatial processes and succession. _Vegetatio 51_: 129–140.
#' * Konrád KD, Bede-Fazekas Á, Bartha S, Somodi I (2023) Adapting a multiscale
#' approach to assess the compositional diversity of landscapes. _Landsc Ecol_
#' _38_: 2731–2747.
#'
#'@export
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
  if(any(apply(x, MARGIN = 2, FUN = is.logical))){
    x <- apply(x, MARGIN = 2, FUN = as.integer)
    warning("Parameter 'x' was found holding a logical data though integer is required. Coercion is done.")
  }
  if(any(apply(x, MARGIN = 2, FUN = function(column){class(column) %in% c("character", "factor")}))){
    x <- apply(x, MARGIN = 2, FUN = function(column){as.integer(as.character(column))})
    warning("Parameter 'x' was found holding data of class character though integer is required. Coercion is done.")
  }
  if(!all(sort(unique(unlist(x))) %in% c(0,1)) ) stop(paste0("Parameter 'x' should contain binary data, with 0 (absence) and 1 (presence) data. However, other values are found: ", paste0(utils::head(sort(unique(unlist(x))), 10), collapse = ", ")))
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
    if(future::supportsMulticore()){
      future::plan("future::multicore")
    }else{
      future::plan("future::multisession")
    }
  } else {
    future::plan("future::sequential")
  }
  default_maxGlobalSize <- options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
  on.exit(expr = future::plan(default_strategy_of_plan), add = TRUE)
  on.exit(expr = options(future.globals.maxSize = default_maxGlobalSize), add = TRUE)
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
        n_workers <- future::nbrOfWorkers()
        n_elements <- length(buffer)
        fold_length <- floor(n_elements / n_workers)
        split_vector <- rep(x = 1:n_workers, times = c(rep(x = fold_length, times = n_workers - 1), n_elements - fold_length * (n_workers - 1)))
        split_results <- future.apply::future_lapply(X = split(buffer, split_vector), function(buffer_split) sf::st_contains(x = buffer_split, y = centroid))
        covered_gridcellIDs <- do.call("c", split_results)
        names(covered_gridcellIDs) <- NULL
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
				 if(length(x)>1){ apply(x[gridcells_to_aggregate,], MARGIN = 2, FUN = base::max) # KKD modositotta 2023 01 04
         }else{ max(x[gridcells_to_aggregate,])}# KKD modositotta 2023 01 04
      })

      aggregated_data <- as.data.frame(do.call(what = base::rbind, args = aggregated_data_list))
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
    combinations <- base::apply(aggregated_data, MARGIN = 1, FUN = paste0, collapse="")
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
  return(results)
}

