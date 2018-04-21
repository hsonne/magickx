# image_matrix -----------------------------------------------------------------

#' Arrange Images in a Matrix
#' 
#' @param images object of class "magick" representing one or more images
#' @param nrow number of rows
#' @param ncol number of columns
#' @param byrow logical. Shall images be arranged along rows or along columns?
#' @param dimnames list of vectors of row and column names, respectively
#' 
#' @export
#' 
image_matrix <- function(
  images, nrow = NULL, ncol = NULL, byrow = FALSE, dimnames = NULL
)
{
  # Get the list of arguments that were passed to this function. Remove the 
  # first element (function call)
  arguments <- as.list(match.call())[-1] 
  
  # Exclude argument "images"
  arguments <- arguments[setdiff(names(arguments), "images")]
  
  # Call matrix() with a sequence between one and the number of images as the 
  # matrix values and the arguments passed to this function
  indices <- do.call(matrix, c(list(seq_along(images)), arguments))
  
  # Combine images horizontally to rows
  row_list <- lapply(seq_len(nrow(indices)), function(i) {
    
    magick::image_append(images[indices[i, ]])
  })

  # Combine row images vertically
  image <- magick::image_append(list_to_magick(row_list), stack = TRUE)
  
  # Set attribute "matrix"
  class(image) <- unique(c("matrix_image", class(image)))

  # Get the heights of the row images
  heights <- sapply(row_list, image_properties, "height")
  
  # Treat the widths of the first row as the widths of the matrix columns
  widths <- image_properties(images[indices[1, ]], "width")
  
  # Set attributs width and height
  structure(image, indices = indices, widths = widths, heights = heights)
}

# image_properties -------------------------------------------------------------
image_properties <- function(images, property)
{
  unname(sapply(
    X = images, 
    FUN = function(image) magick::image_info(image)[[property]]
  ))
}

# dim.matrix_image -------------------------------------------------------------

#' Dimension of a "Matrix Image"
#' 
#' @param x object of class "matrix_image", as returned by 
#'   \code{\link{image_matrix}}
#' 
#' @return vector of two integers (number of rows, number of columns)
#' 
#' @export
#' 
dim.matrix_image <- function(x)
{
  dim(attr(x, "indices"))
}

# [.matrix_image ---------------------------------------------------------------

#' Extract Rows and Columns from "Matrix Image"
#' 
#' @param x object of class "matrix_image"
#' @param \dots arguments passed to \code{\link{extract}}
#' 
#' @return object of class "image-magick"
#' 
#' @export
#' 
"[.matrix_image" <- function(x, ...)
{
  extract(x, ...)
}

# extract ----------------------------------------------------------------------

#' Extract Horizontal or Vertical Stripes from Image
#' 
#' @param image object of class "matrix_image"
#' @param i row indices
#' @param j column indices
#' @param widths column widths in pixels
#' @param heights row heights in pixels
#' 
extract <- function(image, i = NULL, j = NULL, widths = NULL, heights = NULL)
{
  stopifnot(inherits(image, "matrix_image"))
  
  i <- if (missing(i)) NULL else i
  
  j <- if (missing(j)) NULL else j
  
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
