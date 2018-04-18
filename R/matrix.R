# extract ----------------------------------------------------------------------

#' Extract Horizontal or Vertical Stripes from Image
#' 
#' @param image object of class "image-magick"
#' @param i row indices
#' @param j column indices
#' @param widths column widths in pixels
#' @param heights row heights in pixels
#' 
#' @export
#' 
extract <- function(image, i, j, widths = NULL, heights = NULL)
{
  if (is.null(widths)) {
    
    widths <- attr(image, "widths")
  }
  
  if (is.null(heights)) {

    heights <- attr(image, "heights")
  }
  
  select_image_cols(select_image_rows(image, heights, i), widths, j)
}

# select_image_rows ------------------------------------------------------------
select_image_rows <- function(image, heights, indices = NULL)
{
  select_image_stripes(image, heights, indices, horizontal = TRUE)
}

# select_image_cols ------------------------------------------------------------
select_image_cols <- function(image, widths, indices = NULL)
{
  select_image_stripes(image, widths, indices, horizontal = FALSE)
}

# select_image_stripes ---------------------------------------------------------
select_image_stripes <- function(
  image, sizes, indices = NULL, horizontal = FALSE
)
{
  if (is.null(indices)) {
    
    image
    
  } else {
    
    geometries <- to_crop_dimensions(sizes, indices)
    
    geometry_strings <- apply(geometries, 1, function(g) if (horizontal) {
      magick::geometry_area(0, g["n_pixels"], 0, g["offset"])
    } else {
      magick::geometry_area(g["n_pixels"], 0, g["offset"], 0)
    })
    
    column_images <- lapply(geometry_strings, magick::image_crop, image = image)
    
    magick::image_append(do.call(c, column_images), stack = horizontal)
  } 
}

# to_crop_dimensions -----------------------------------------------------------
to_crop_dimensions <- function(sizes, indices = seq_along(sizes))
{
  ranges <- to_pixel_ranges(sizes, indices)

  cbind(
    offset = ranges[, "from"], 
    n_pixels = ranges[, "to"] - ranges[, "from"] + 1
  )
}

# to_pixel_ranges --------------------------------------------------------------
to_pixel_ranges <- function(sizes, indices)
{
  ranges <- indices_to_ranges(indices, max_index = length(sizes))
  
  areas <- to_areas(x = sizes)
  
  from_to <- do.call(rbind, lapply(seq_len(nrow(ranges)), function(i) {
    
    c(areas[ranges[i, "from"], "from"], areas[ranges[i, "to"], "to"])
  }))
}

# indices_to_ranges ------------------------------------------------------------
indices_to_ranges <- function(indices, max_index = 0)
{
  #indices <- c(3:4, 7:9, 12); max_index <- 15
  
  if (any(indices < 0)) {
    
    stopifnot(all(indices < 0))
    stopifnot(max_index >= max(abs(indices)))
    
    indices <- setdiff(seq_len(max_index), abs(indices))
  }
  
  gap_positions <- which(diff(indices) > 1)

  from <- c(indices[1], indices[gap_positions + 1])
  
  to <- c(indices[gap_positions], indices[length(indices)])
  
  cbind(from = from, to = to)
}

# to_areas ---------------------------------------------------------------------
to_areas <- function(x)
{
  cum_sum <- cumsum(x)
  
  cbind(from = c(1, cum_sum[- length(cum_sum)] + 1), to = cum_sum)
}
