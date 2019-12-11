library(grid)

# phi <- (sqrt(5) + 1) / 2

nstar <- function(vert = 5, rtio = NA, color = "blue", fill = "blue") {

  if (vert < 0) vert <- -vert
  pi_dvert <- pi / (2 * vert)

  if (is.na(rtio)) {
    # Izračunaj ramerje med polmeroma notranjega in zunanjega kroga za pravilno zvezdo
    # (vert - 4) * pi / (2 * vert) => pol vrha kraka zvezde
    # (vert + 2) * pi / (2 * vert) => "tretji" kot
    rtio <- sin((vert - 4) * pi_dvert) / sin((vert + 2) * pi_dvert)
    }
  if (rtio > 1) rtio <- 1 / rtio
  b <- c(0:(2 * vert - 1)) * 2 * pi_dvert + pi/2
  step <- c(rep(c(1, rtio), vert))

  x <- (cos(b) * step)
  y <- (sin(b) * step)

  # polygonGrob(x, y, default.units = "native", vp = vp)
  grid.polygon(x, y, default.units = "native", vp = vp)
}

# Point shapes:
#
# triangle, invtriangle, star3
# square, diamond, diamond_slim,  star4
# pentagon, star5, star5slim, star5fat, star5double,
# hexagon, star6, star6slim, star6double
# star8, star8double

triangle <- function() {
  b <- c(0:2) * 2 * pi / 3 + pi/2
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

invtriangle <- function() {
  b <- c(0:2) * 2 * pi / 3 + 3 * pi/2
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

square <- function() {
  b <- c(0:3) * 2 * pi / 4 + pi/4
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

diamond <- function() {
  b <- c(0:3) * 2 * pi / 4 + pi/2
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

pentagon <- function() {
  b <- c(0:4) * 2 * pi / 5 + pi/2
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

hexagon <- function() {
  b <- c(0:4) * 2 * pi / 5 + pi/2
  # polygonGrob((cos(b)), (sin(b)), default.units = "native", vp = vp)
  grid.polygon((cos(b)), (sin(b)), default.units = "native", vp = vp)
}

star3 <- function() {
  nstar(3, 0.5)
}

diamondslim <- function() {
  nstar(2, 0.75)
}

star4 <- function() {
  nstar(4, 0.5)
}

star5slim <- function() {
  nstar(5, 0.3)
}

star5 <- function() {
  nstar(5)
}

star5fat <- function() {
  nstar(5, .6)
}

star5double <- function() {
  nstar(5, -1.5)
}

star6 <- function() {
  nstar(6)
}

star6slim <- function() {
  nstar(6, 0.3)
}

star6double <- function() {
  nstar(5, -1)
}

star8slim <- function() {
  nstar(8, 0.5)
}

star8double <- function() {
  nstar(8, -0.6)
}

vp <- viewport(xscale = c(-1, 1), yscale = c(-1, 1), name = "ta_view")
pushViewport(vp)
nstar(5, -1)
nstar(6, sqrt(3))
nstar(7, -1)



triangle()
invtriangle()
star3()

square()
diamond()
star4()

pentagon()
star5()
star5slim()
star5fat()
star5double()

hexagon()
star6()
star6slim()
star6double()

star8()
star8double()

for (i in c(8:5)) {
  nstar(i)
}
for (i in c(3:10)) {
  nstar(i, 2- phi)
}
for (i in c(3:10)) {
  nstar(i, -i/2)
}

