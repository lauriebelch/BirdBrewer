
### colours selected with Adobe Color using 'Extract Theme' & 'Colorful' theme
## available at https://color.adobe.com/create/image

### images download from unsplah (freely-usable images)
## available at https://unsplash.com/

### code based on R package 'wesanderson'
## available at https://github.com/karthik/wesanderson

#' Complete list of palettes
#'
#' Use \code{\link{wes_palette}} to construct palettes of desired length.
#'
#' @export
bird_palettes <- list(
  Kingfisher = c("#200D06", "#00B1B3", "#A4FFFF", "#FFA31A", "#D12300"),
  BlueTit = c("#8C96CB", "#40456B", "#C39B3B", "#5E5C4D"),
  ChafFinch = c("#638BBF", "#1E2537", "#7E8776", "#E5AD96"),
  GoldFinch = c("#621016", "#79512D", "#D9AE30", "#E29A0A"),
  GreatTit = c("#131514", "#939554", "#D5CE7D", "#424B5A"),
  HerringGull = c("#F29F05", "#D97904", "#8C3503", "#590202"),
  Kestrel = c("#38271D", "#8C7C6F", "#D6BF7C", "#BF9278"),
  Mallard = c("#232609", "#00534C", "#02733E", "#04BF68"),
  Mandarin = c("#BD363C", "#F55981", "#F2984E", "#5F4C7A", "#002675"),
  Parakeet = c("#F25252", "#579280", "#C1D90B", "#B4BF60", "#EBEABB"),
  Pheasant = c("#400D01", "#BF0426", "#BF73AB", "#F28444"),
  Robin = c("#783B0F", "#AE6846", "#F48246", "#F69548"),
  Shoveller = c("#102E38", "#456F23", "#8C2703", "#6E8897"),
  Sparrowhawk = c("#3F3529", "#A0866B", "#7B4636", "#5872A6"),
  WoodPigeon = c("#7F848A", "#93B1B1", "#D0D0B8", "#FFD998", "#F2F2F2")
)
#' A palette generator based on BIRDS
#'
#' These are a handful of color palettes from photos of birds.
#'
#' @param n Number of colors desired. Most palettes have
#'   4 or 5 colors.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{Kingfisher}, \code{BlueTit},  \code{ChafFinch},
#'   \code{GoldFinch}, \code{GreatTit},  \code{HerringGull}, \code{Kestrel},
#'   \code{Mallard},  \code{Mandarin} , \code{Parakeet} ,
#'   \code{Pheasant}, \code{Robin}, \code{Shoveller}, \code{Sparrowhawk},
#'   \code{WoodPigeon}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' bird_palette("WoodPigeon")
#' bird_palette("Mandarin")
#' bird_palette("Parakeet")
#' bird_palette("HerringGull", 3)
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- wes_palette(21, name = "HerringGull", type = "continuous")
#' image(volcano, col = pal)
bird_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  pal <- bird_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}
#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}