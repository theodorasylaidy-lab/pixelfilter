#' pixelfilter: simple colour filters for images
#'
#' This package changes the colours of an image pixel by pixel.
#'
#' How to use it:
#' 1. `load_bitmap()` reads the image and gives the raw bitmap
#'    (array with `[colour, x, y]`).
#' 2. Choose a filter: `grey_filter()`, `red_filter()`, `green_filter()`,
#'    `blue_filter()` or `cutoff_filter()`. A filter changes one pixel.
#' 3. `apply_filter()` runs the filter on every pixel.
#' 4. `show_bitmap()` shows the new image.
#'
#' There is also a real fluorescence staining image (`example_image()`) and
#' `stained_area()`, which uses the cut-off idea to measure how much of the
#' image is stained, in total or for one colour.
#'
#' @examples
#' img <- load_bitmap(example_image())
#' grey <- apply_filter(img, grey_filter)
#' show_bitmap(grey)
#'
#' # only the red marker
#' show_bitmap(apply_filter(img, red_filter))
#' stained_area(img, cutoff = 150, channel = "red")
#' @keywords internal
"_PACKAGE"


#' Load an image as a bitmap
#'
#' @param path file path to the image (for example jpeg or png)
#' @return raw array with dimensions `[colour, x, y]`
#' @examples
#' \dontrun{
#' bmp <- load_bitmap("image.jpg")
#' }
#' @export
load_bitmap <- function(path) {
  if (!file.exists(path)) {
    stop("file does not exist")
  }
  img <- magick::image_read(path)
  bitmap <- img[[1]]
  return(bitmap)
}


#' Show a bitmap as an image
#'
#' @param bitmap raw array from `load_bitmap()` or `apply_filter()`
#' @return magick image (it is shown in the viewer)
#' @examples
#' \dontrun{
#' show_bitmap(load_bitmap("image.jpg"))
#' }
#' @export
show_bitmap <- function(bitmap) {
  magick::image_read(bitmap)
}


#' Grey filter
#'
#' Makes one pixel grey with R * 0.299 + G * 0.587 + B * 0.114.
#'
#' @param pixel raw vector with red, green and blue (and maybe alpha)
#' @return the new pixel as raw vector
#' @examples
#' grey_filter(as.raw(c(50, 120, 10)))
#' @export
grey_filter <- function(pixel) {
  rgb <- as.integer(pixel[1:3])
  grey <- round(rgb[1] * 0.299 + rgb[2] * 0.587 + rgb[3] * 0.114)
  pixel[1:3] <- as.raw(grey)
  return(pixel)
}


#' Red, green and blue filters
#'
#' Keeps only one colour of the pixel and sets the others to 0.
#'
#' @param pixel raw vector with red, green and blue (and maybe alpha)
#' @return the new pixel as raw vector
#' @examples
#' red_filter(as.raw(c(50, 120, 10)))
#' green_filter(as.raw(c(50, 120, 10)))
#' blue_filter(as.raw(c(50, 120, 10)))
#' @name colour_filters
NULL

#' @rdname colour_filters
#' @export
red_filter <- function(pixel) {
  pixel[c(2, 3)] <- as.raw(0)
  return(pixel)
}

#' @rdname colour_filters
#' @export
green_filter <- function(pixel) {
  pixel[c(1, 3)] <- as.raw(0)
  return(pixel)
}

#' @rdname colour_filters
#' @export
blue_filter <- function(pixel) {
  pixel[c(1, 2)] <- as.raw(0)
  return(pixel)
}


#' Cut-off filter
#'
#' Takes the grey value of the pixel. If it is lower than the cut-off the
#' pixel gets the `low` colour, otherwise the `high` colour.
#'
#' @param pixel raw vector with red, green and blue (and maybe alpha)
#' @param cutoff number between 0 and 255 (default 127, which is 50%)
#' @param low colour for dark pixels (hex code)
#' @param high colour for light pixels (hex code)
#' @return the new pixel as raw vector
#' @examples
#' cutoff_filter(as.raw(c(50, 120, 10)))
#' cutoff_filter(as.raw(c(200, 200, 200)), cutoff = 100)
#' @export
cutoff_filter <- function(pixel, cutoff = 127, low = "#000000", high = "#FF00FF") {
  grey <- as.integer(grey_filter(pixel)[1])
  if (grey < cutoff) {
    new_colour <- low
  } else {
    new_colour <- high
  }
  pixel[1:3] <- as.raw(grDevices::col2rgb(new_colour)[, 1])
  return(pixel)
}


#' Apply a filter to the whole image
#'
#' Goes through the bitmap pixel by pixel and uses the filter on each pixel.
#'
#' @param bitmap raw array from `load_bitmap()`
#' @param filter filter function, for example `grey_filter`
#' @param ... extra arguments for the filter, for example `cutoff = 100`
#' @return the new bitmap
#' @examples
#' bmp <- array(as.raw(c(50, 120, 10, 255, 255, 255)), dim = c(3, 2, 1))
#' apply_filter(bmp, grey_filter)
#' apply_filter(bmp, cutoff_filter, cutoff = 100)
#' @export
apply_filter <- function(bitmap, filter, ...) {
  if (!is.function(filter)) {
    stop("filter has to be a function")
  }
  new_bitmap <- bitmap
  for (x in 1:dim(bitmap)[2]) {
    for (y in 1:dim(bitmap)[3]) {
      new_bitmap[, x, y] <- filter(bitmap[, x, y], ...)
    }
  }
  return(new_bitmap)
}


#' Path to the example staining image
#'
#' The package comes with a real fluorescence staining image to try the
#' filters on. Blue shows the nuclei, red/pink shows a marker that only
#' some cells have, and white shows a third staining.
#'
#' @return file path to `staining.png`
#' @examples
#' img <- load_bitmap(example_image())
#' dim(img)
#' @export
example_image <- function() {
  system.file("extdata", "staining.png", package = "pixelfilter")
}


#' Fraction of the image that is stained
#'
#' Counts how many pixels are at or above the cut-off and gives it as a
#' percentage of the whole image. With `channel = "grey"` it uses the grey
#' value, otherwise only the red, green or blue value. For example
#' `channel = "red"` gives the area with the red marker. White pixels (all
#' three colours high) are not counted for one colour, because white is
#' not the marker. This is called thresholding and is a common measurement
#' in image analysis.
#'
#' @param bitmap raw array from `load_bitmap()`
#' @param cutoff number between 0 and 255 (default 127)
#' @param channel "grey", "red", "green" or "blue"
#' @return percentage of pixels at or above the cut-off
#' @examples
#' img <- load_bitmap(example_image())
#' stained_area(img, cutoff = 120)                  # all staining
#' stained_area(img, cutoff = 150, channel = "red") # red marker
#' stained_area(img, cutoff = 150, channel = "blue") # nuclei
#' @export
stained_area <- function(bitmap, cutoff = 127, channel = "grey") {
  if (!channel %in% c("grey", "red", "green", "blue")) {
    stop("channel has to be grey, red, green or blue")
  }
  above <- 0
  for (x in 1:dim(bitmap)[2]) {
    for (y in 1:dim(bitmap)[3]) {
      pixel <- bitmap[, x, y]
      rgb <- as.integer(pixel[1:3])
      if (channel == "grey") {
        stained <- as.integer(grey_filter(pixel)[1]) >= cutoff
      } else {
        i <- switch(channel, red = 1, green = 2, blue = 3)
        white <- all(rgb >= cutoff)
        stained <- rgb[i] >= cutoff && !white
      }
      if (stained) {
        above <- above + 1
      }
    }
  }
  total <- dim(bitmap)[2] * dim(bitmap)[3]
  return(round(above / total * 100, 1))
}
