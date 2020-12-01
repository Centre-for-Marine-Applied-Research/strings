#'@title Converts values in the DEPTH column to an ordered factor

#'@description This function simplifies converting the DEPTH column from class
#'  \code{numeric}, \code{factor}, or \code{character} to an ordered factor,
#'  which is preferred for using the function \code{plot_variables_at_depth()}.
#'@details To use the function \code{plot_variables_at_depth()}, \code{DEPTH}
#'  must be a factor because \code{DEPTH} is mapped to a discrete color scale.
#'  For the legend to display correctly (e.g. depths arranged from shallow to
#'  deep), \code{DEPTH} must be an \emph{ordered} factor.
#'
#'  The \code{DEPTH} column may be values that can be coerced to class
#'  \code{numeric}, qualitative descriptors, or a combination of both. A
#'  different level will be assigned to each of the unique values of
#'  \code{DEPTH}. This could cause some confusion when \code{DEPTH} includes
#'  numeric and qualitative values, for example because the level (colour) for
#'  "Surface" will not the same as the level for "2 m").
#'
#'  Qualitative \code{DEPTH} values will be standardized to "Surface",
#'  "Sub-surface", "Middle", and "Bottom". Accepted qualitative \code{DEPTH}
#'  values are "Bottom", "bottom", "Middle", "middle", "Sub surface",
#'  "Sub-Surface", Sub-surface", "sub surface", "sub-surface", "Sub", "sub",
#'  "Surface", and "surface". Other values will cause the function to stop with
#'  an error message.
#'
#'  For numeric depths: the smallest value is assigned to the first level, the
#'  second smallest value is assigned to the second level, and so on.
#'
#'  For qualitative depths: the assigned order is "Surface" < "Sub-surface" <
#'  "Middle" < "Bottom".
#'
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include at least one column: \code{DEPTH}
#'  (factor, character, or numeric).
#'@return Returns \code{dat.tidy}, with the \code{DEPTH} column converted to an
#'  ordered factor.
#'@family format
#'@author Danielle Dempsey
#'@importFrom dplyr mutate arrange
#'@export

convert_depth_to_ordered_factor <- function(dat.tidy){

  # add a temporary column.
  # assign TRUE if DEPTH is a qualitative value and FALSE if DEPTH can be converted to a numeric value
  dat.tidy <- dat.tidy %>%
    mutate(DEPTH_QUAL = if_else(is.na(suppressWarnings(as.numeric(DEPTH))), TRUE, FALSE))

  # keep only qualitative depths
  dat.qual <- dat.tidy %>%
    filter(DEPTH_QUAL == TRUE) %>%
    select(-DEPTH_QUAL)

  # keep only numeric depths
  dat.num <- dat.tidy %>%
    filter(DEPTH_QUAL == FALSE) %>%
    select(-DEPTH_QUAL)

  # if there is qualitative DEPTH data:
  if(nrow(dat.qual) > 0) {

    # stop with error message if any of the qualitative DEPTH values are not recognized by the function
    accepted.depths <- c("bottom", "Bottom", "middle", "Middle",
                         "sub-surface", "Sub-Surface", "Sub Surface", "Sub", "sub", "Sub-surface",
                         "surface", "Surface")
    depths <- unique(dat.qual$DEPTH)

    if(any(depths %in% accepted.depths == FALSE)) {

      bad.depth <- depths[which(!(depths %in% accepted.depths))]
      stop(paste("There is an unrecognized qualitative DEPTH value in dat.tidy: ", bad.depth, sep = ""))

    }

    # standardize DEPTH values and assign levels
    dat.qual <- dat.qual %>%
      mutate(DEPTH = case_when(DEPTH == "bottom" ~ "Bottom",
                               DEPTH == "middle" ~ "Middle",
                               DEPTH == "sub-surface" | DEPTH == "Sub-Surface"|
                                 DEPTH == "Sub Surface" | DEPTH == "Sub" |
                                 DEPTH == "sub"  ~ "Sub-surface",
                               DEPTH == "surface" ~ "Surface",
                               TRUE ~ DEPTH)) %>%
      mutate(DEPTH = factor(DEPTH)) %>%
      mutate(DEPTH = ordered(DEPTH,
                             levels = c( "Surface", "Sub-surface", "Middle", "Bottom"))) %>%
      arrange(DEPTH)

  }

  # if there is numeric DEPTH data, assign levels
  if(nrow(dat.num) > 0) {

    dat.num <- dat.num %>%
      mutate(DEPTH = factor(DEPTH)) %>%
      # assign levels to the factor based on the numeric values of DEPTH
      mutate(DEPTH = ordered(DEPTH,
                             levels = as.numeric(levels(DEPTH))[order(as.numeric(levels(DEPTH)))])) %>%
      arrange(DEPTH)
  }

  rbind(dat.qual, dat.num)
}



