#'Number of grid cells covered by enlarged spatial units
#'
#'Calculate the number of grid cells covered by the radially enlarged spatial
#'units.
#'
#'@param aggregation_step A numeric vector of length one containing a
#'  non-negative number. This number expresses the size of the spatial units for
#'  which calculation of compositional diversity and associatum is required. The
#'  size is measured by the number of rows of grid cells around the central grid
#'  cell, where 0 means the original cell without enlargement. Fraction number
#'  can be also set as input. In this case, the following step's spatial unit
#'  minus grid cells touching the vertices are used as spatial base units. Note,
#'  in the case of hexagonal grid, steps falling in the interval ]0,1[ cannot be
#'  evaluated. Negative, non-finite and missing values are ignored with warning.
#'@param square A logical vector of length one. Should be `TRUE` if square grid,
#'  `FALSE`, if is hexagonal grid is used as the basis of calculation.
#'
#'@return A numeric vector of length one.
#'@export
#'
#'@examples
#' calculate_SpatialUnitSize(aggregation_step = 0, square = TRUE)
#'
#' foo = c()
#' for(i in c(0, 0.5, 1, 1.5, 2, 3))
#'   foo = c(foo, calculate_SpatialUnitSize(aggregation_step = i, square = TRUE))
#' foo
#'
#' vapply(c(0, 1, 1.5, 2, 3), FUN = function(i){
#'   calculate_SpatialUnitSize(aggregation_step = i, square = FALSE)},
#'   FUN.VALUE = numeric(length = 1))
#'
calculate_SpatialUnitSize = function(aggregation_step, square = FALSE){
  # Checking parameters
  if(missing(aggregation_step)) stop("Parameter 'aggregation_step' should be set.")
  if(any(is.na(aggregation_step))){
    aggregation_step = aggregation_step[!is.na(aggregation_step)]
    warning("NA values provided to parameter 'aggregation_step' are ignored.")
  }
  if(any(!is.finite(aggregation_step))){
    aggregation_step = aggregation_step[is.finite(aggregation_step)]
    warning("Infinite values provided to parameter 'aggregation_step' are ignored.")
  }
  if(length(aggregation_step)>1){
    warning("Parameter 'aggregation_step' has more elements (", as.character(length(aggregation_step)), ") then expected (1). Only the first element is used.")
    aggregation_step <- aggregation_step[1]
  } else if(length(aggregation_step)<1) stop("Parameter 'aggregation_step' should be of length one.")
  if(aggregation_step < 0) stop("Parameter 'aggregation_step' should be non-negative.")
  if(!is.numeric(aggregation_step)) stop("Parameter 'aggregation_step' should be a numeric.")
  if(length(square)>1){
    warning("Parameter 'square' has more elements (", as.character(length(square)), ") then expected (1). Only the first element is used.")
    square <- square[1]
  } else if(length(square)<1) stop("Parameter 'square' should be of length one.")
  if(!is.logical(square)) stop("Parameter 'square' should be logical.")
  if(is.na(square)) stop("NA values provided to parameter 'square'.")
  if(aggregation_step >0 & aggregation_step <1 & !square) stop("Value (", aggregation_step,") set to parameter 'aggregation_steps' falling in the range ]0,1[ cannot be evaluated when using hexagonal grid.")

  # Calculation
  aggregation_step_is_integer = aggregation_step %% 1 == 0
  if(square){ # square grid
    count <- (ceiling(aggregation_step)*2+1)^2           # formula to use if the given aggregation_step is integer
    if(!aggregation_step_is_integer) count <- count - 4 # formula to use if the given aggregation_step is fraction
  }else{ # hexagonal grid
    count <- sum((0:ceiling(aggregation_step)) * 6) + 1 # formula to use if the given aggregation_step is integer
    if(!aggregation_step_is_integer) count <- count - 6 # formula to use if the given aggregation_step is fraction
  }
  count
}
