#'@title Converts values in the DEPTH column to an ordered factor
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include at least one column: \code{DEPTH}
#'  (factor, character, or numeric). The values of \code{DEPTH} must be
#'  coercible to class \code{numeric}.
#'@description This function simplifies converting the DEPTH column from class
#'  \code{numeric}, \code{factor}, or \code{character} to an ordered factor,
#'  which is preferred for using the function \code{plot_variables_at_depth()}.
#'@details To use the function \code{plot_variables_at_depth()}, \code{DEPTH}
#'  must be a factor because \code{DEPTH} is mapped to a discrete color scale.
#'  For the legend to display correctly (e.g. depths arranged from shallow to
#'  deep), \code{DEPTH} must be an ordered factor.
#'@return Returns dat.tidy, with the DEPTH column converted to an ordered
#'  factor. The smallest value will be assigned to the first level, the second
#'  smallest value will be assigned to the second level, and so on.
#'@family format
#'@author Danielle Dempsey
#'@importFrom dplyr mutate arrange
#'@export

convert_depth_to_ordered_factor <- function(dat.tidy){


  # dat.tidy <- dat.tidy %>%
  #   mutate(DEPTH_QUAL = if_else(is.na(suppressWarnings(as.numeric(DEPTH))),
  #                               as.character(DEPTH), NA_character_))

  # filter out where DEPTH_QUAL = NA and do the numeric thing
  # filter out where DEPTH_QUAL != NA and see the possible values of written depth
  #

  if(is.na(suppressWarnings(as.numeric(dat.tidy$DEPTH[1])))) {

    stop("Column DEPTH cannot be coerced to class numeric")
  }

  dat.out <- dat.tidy %>%
    mutate(DEPTH = factor(DEPTH)) %>%  # convert DEPTH to factor
    # assign levels to the factor based on the numeric values of DEPTH
    mutate(DEPTH = ordered(DEPTH,
                           levels = as.numeric(levels(DEPTH))[order(as.numeric(levels(DEPTH)))])) %>%
    arrange(DEPTH)

  dat.out
}



