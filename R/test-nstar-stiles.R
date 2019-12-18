library(grid)

nstarn <- function(vert = 5, ratio = NA) {

  if (vert < 0) vert <- -vert
  pi_dvert <- pi / (2 * vert)

  if (is.na(ratio)) {
    # Izračunaj ramerje med polmeroma notranjega in zunanjega kroga za pravilno zvezdo
    # (vert - 4) * pi / (2 * vert) => pol vrha kraka zvezde
    # (vert + 2) * pi / (2 * vert) => "tretji" kot
    ratio <- sin((vert - 4) * pi_dvert) / sin((vert + 2) * pi_dvert)
  }

  if (ratio > 1) ratio <- 1 / ratio
  b <- c(0:(2 * vert - 1)) * 2 * pi_dvert + pi/2
  step <- c(rep(c(1, ratio), vert))

  xy <- exp(1i * b)
  x <- (cbind(Re(xy), Im(xy)) * step + 1) / 2
  # polygonGrob(x, y, default.units = "native")
  grid.polygon(x[, 1], x[, 2], default.units = "npc")
}

nstaro <- function(vert = 5, ratio = NA) {

  if (vert < 0) vert <- -vert
  pi_dvert <- pi / (2 * vert)

  if (is.na(ratio)) {
    # Izračunaj ramerje med polmeroma notranjega in zunanjega kroga za pravilno zvezdo
    # (vert - 4) * pi / (2 * vert) => pol vrha kraka zvezde
    # (vert + 2) * pi / (2 * vert) => "tretji" kot
    ratio <- sin((vert - 4) * pi_dvert) / sin((vert + 2) * pi_dvert)
  }
  if (ratio > 1) ratio <- 1 / ratio
  b <- c(0:(2 * vert - 1)) * 2 * pi_dvert + pi/2
  step <- c(rep(c(1, ratio), vert))

  x <- cbind(
    ((cos(b) * step) + 1) / 2,
    ((sin(b) * step) + 1) / 2)

  # polygonGrob(x, y, default.units = "native")
  grid.polygon(x[, 1], x[, 2], default.units = "npc")
}

nstar <- function(vert = 5, ratio = NA, default.units = "npc", gp = gpar()) {

  if (vert < 0) vert <- -vert
  pi_dvert <- pi / (2 * vert)

  if (is.na(ratio)) {
    # Izračunaj ramerje med polmeroma notranjega in zunanjega kroga za pravilno zvezdo
    # (vert - 4) * pi / (2 * vert) => pol vrha kraka zvezde
    # (vert + 2) * pi / (2 * vert) => "tretji" kot
    ratio <- sin((vert - 4) * pi_dvert) / sin((vert + 2) * pi_dvert)
  }
  if (ratio > 1) ratio <- 1 / ratio
  b <- c(0:(2 * vert - 1)) * 2 * pi_dvert + pi/2
  step <- c(rep(c(1, ratio), vert))

  x <- ((cos(b) * step) + 1) / 2
  y <- ((sin(b) * step) + 1) / 2

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  l <- list(x=x, y=y, gp=gp, vp=vp)
  cl <- "lines"
  grob(l, cl, draw = TRUE)
}

n <- 1000
system.time(for (i in 1:n) nstaro())
system.time(for (i in 1:n) nstarn())
system.time(for (i in 1:n) nstar())

