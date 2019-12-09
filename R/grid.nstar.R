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

  x <- (cos(b) * step) # / 2 + 0.5
  y <- (sin(b) * step) # / 2 + 0.5

  # polygonGrob(x, y, default.units = "native", vp = vp)
  grid.polygon(x, y, default.units = "native", vp = vp)
}


vp <- viewport(xscale = c(-1, 1), yscale = c(-1, 1), name = "ta_view")
pushViewport(vp)
nstar(5)

for (i in c(8:5)) {
  nstar(i)
}
for (i in c(3:10)) {
  nstar(i, 2- phi)
}
for (i in c(3:10)) {
  nstar(i, -i/2)
}

vp <- viewport(x=0.5,y=0.5,width=0.9, height=0.9)
