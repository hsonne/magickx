# try_image_read ---------------------------------------------------------------

#' Try to Read Images with image_read
#' 
#' Run \code{\link[magick]{image_read}} for each element of \code{path}. If 
#' an error occurs, continue with the next element. 
#' 
#' @param path passed to \code{\link[magick]{image_read}}
#'
#' @return object of class "magick-image", containing all images that could
#'   be read successfully from the paths given in \code{path}
#'   
#' @export
#' 
try_image_read <- function(path)
{
  # Try to read the images from the URLs
  image_list <- lapply(path, function(p) {
    
    result <- try(magick::image_read(p), silent = TRUE)
    
    if (inherits(result, "try-error")) {
      
      message(result)
    }
    
    result
  })
  
  # Exclude list elements that represent files that could not be loaded
  list_to_magick(x = image_list[! sapply(image_list, inherits, "try-error")])
}

# list_to_magick ---------------------------------------------------------------

#' Convert List of image-magick Objects to image-magick Object
#' 
#' @param x list of objects of class "image-magick"
#' 
#' @return object of class "image-magick"
#' 
#' @export
#' 
list_to_magick <- function(x)
{
  stopifnot(is.list(x), all(sapply(x, inherits, "magick-image")))
  
  do.call(magick::image_join, x)
}
