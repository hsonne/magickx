# crop_to_column ---------------------------------------------------------------

#' Append Cropped Areas From Images Vertically
#' 
#' @param images object of class "magick-image" representing one or more images
#' @param geometry geometry string
#' 
#' @return object of class "magick-image" 
#' 
#' @export
#' 
crop_to_column <- function(images, geometry)
{
  stopifnot(is.character(geometry))
  
  if (length(geometry) > 1) {
    
    images <- lapply(geometry, crop_to_column, images = images)

    return(structure(
      magick::image_append(do.call(c, images)), 
      widths = image_properties(images, "width"),
      heights = attr(images[[1]], "height")
    ))
  }
  
  images <- magick::image_crop(images, geometry)
  
  structure(
    magick::image_append(images, stack = TRUE), 
    widths = image_properties(images, "width"),
    heights = image_properties(images, "height")
  )
}

# image_properties -------------------------------------------------------------
image_properties <- function(images, property)
{
  unname(sapply(
    X = images, 
    FUN = function(image) magick::image_info(image)[[property]]
  ))
}

# crop_to_row ------------------------------------------------------------------

#' Append Cropped Areas From Images Horizontally
#' 
#' @param images object of class "magick-image" representing one or more images
#' @param geometry geometry string
#' 
#' @return object of class "magick-image" 
#' 
#' @export
#' 
crop_to_row <- function(images, geometry)
{
  magick::image_append(magick::image_crop(images, geometry), stack = FALSE)
}
