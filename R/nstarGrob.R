library(grid)

##' @aliases nstarGrob grid.nstar
##' @title Regular ngram (star) grob
##' @description Regular ngram (star) with optional rotation and aesthetic attributes.
##' @describeIn nstarGrob return a ngram (star) grob
##' @param x x unit
##' @param y y unit
##' @param n number of vertices
##' @param size radius of circumscribing circle
##' @param phase angle in radians of first point relative to x axis
##' @param position.units default units for the positions
##' @param size.units grid units for the sizes
##' @param gp gpar
##' @param ... further parameters passed to polygonGrob
##' @return A grob.
##' @export
##' @examples
##' library(grid)
##' N <- 5
##' xy <- star_regular(N)*2
##'
##' # draw multiple stars
##' g <- nstarGrob(unit(xy[,1],"cm") + unit(0.5,"npc"),
##'               unit(xy[,2],"cm") + unit(0.5,"npc"),
##'               n = seq_len(N) + 2, gp = gpar(fill=1:N))
##'
##' grid.newpage()
##' grid.draw(g)
##'

# circleGrob <- function(x=0.5, y=0.5, r=0.5,
#                        default.units="npc",
#                        name=NULL, gp=gpar(), vp=NULL)

nstarGrob <- function (x = 0.5, y = 0.5, r = 0.5,
                       default.units = "npc",
                       n = 5, size = 10, phase = 0,                         # size => r
                       gp = gpar(colour = "black", fill = NA,                 # not reallyneeded? default parameters
                                 linejoin = "mitre"), ...,
                       position.units = default.units, size.units="mm")       # merge to default units
{
  N <- length(x)
  stopifnot(length(y) == N)

  if (!is.unit(x))
    x <- unit(x, position.units)
  if (!is.unit(y))
    y <- unit(y, position.units)

  xv <- convertX(x, position.units, TRUE)
  yv <- convertY(y, position.units, TRUE)

  if (length(n) < N)
    n <- rep(n, length.out = N)
  if (length(size) < N)
    size <- rep(size, length.out = N)
  if (length(phase) < N)
    phase <- rep(phase, length.out = N)

  lngon <- mapply(star_regular, n = n, phase = phase,
                  SIMPLIFY = FALSE)
  vertices <- sapply(lngon, nrow)

  rotate_move <- function(p, size, x, y){ # Positioning and rotation (arround here)
    central <- size * p

    list(x = unit(central[,1], size.units) + unit(x, position.units),
         y = unit(central[,2], size.units) + unit(y, position.units))

  }

  lxy <- mapply(rotate_move, p=lngon,
                size=size, x=xv, y=yv,
                SIMPLIFY = FALSE)

  allx <- do.call("unit.c", lapply(lxy, "[[", 1))
  ally <- do.call("unit.c", lapply(lxy, "[[", 2))

  polygonGrob(allx, ally, id.lengths = vertices, gp = gp, ...)

}

#' @describeIn nstarGrob draw a ngram (star) grob on the current device
#' @inheritParams nstarGrob
#' @export
grid.nstar <- function(...)
{
  grid.draw(nstarGrob(...))
}

#' @describeIn nstarGrob return the x,y coordinates of a regular polygon inscribed in the unit circle
#' @inheritParams nstarGrob
#' @export
star_regular <- function(n = 5, phase = 0, ratio = NA){
  stopifnot(n > 2)
  dbl_n <- n * 2
  pi_n <- pi / (dbl_n)

  if (is.na(ratio)) {
    # Calculate a ratio between the radius of the inner and outer circle for the regular ngon (star).
    ratio <- sin((n - 4) * pi_n) / sin((n + 2) * pi_n)
  }
  if (ratio > 1) ratio <- 1 / ratio
  step <- c(rep(c(1, ratio), dbl_n))
  cc <- exp(seq(0, dbl_n-1)*2i*pi/dbl_n) * exp(1i*(phase+pi/2))
  cbind(Re(cc), Im(cc)) * step
}

N <- 8
xy <- star_regular(N / 2, ratio = 1)*2

# draw multiple stars
g <- nstarGrob(unit(xy[,1],"cm") + unit(0.5,"npc"),
              unit(xy[,2],"cm") + unit(0.5,"npc"),
              n = seq_len(N) + 4, size = 5, phase = 0, gp = gpar(fill=1:N))

grid.newpage()
grid.draw(g)
