library(grid)

# phi <- (sqrt(5) + 1) / 2

nstar <- function(vert = 5, ratio = NA) {

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

  # polygonGrob(x, y, default.units = "native")
  grid.polygon(x, y, default.units = "npc")
}

grid.nstar <- function(x=0.5, y=0.5, r=0.5, default.units="npc",
                       name=NULL, gp=gpar(), draw=TRUE, vp=NULL,
                       shape = "", point = NA, ratio = NA) {
  # Posible shapes:
  # triangle, invtriangle, star3
  # square, diamond, diamond_slim,  star4
  # pentagon, star5, star5slim, star5fat, star5double,
  # hexagon, star6, star6slim, star6double
  # star8, star8double,
  # circle
  if (!is.na(point)) shape < NULL
  switch(shape,
         circle = grid.circle(x, y, r, default.units,
                              name, gp, draw, vp),
         triangle =  {
           b <- c(0:2) * 2 * pi / 3 + pi/2
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         invtriangle = {
           b <- c(0:2) * 2 * pi / 3 + 3 * pi/2
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         star3 =  nstar(3, 0.5),
         square =  {
           b <- c(0:3) * 2 * pi / 4 + pi/4
           # polygonGrob(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         diamond =  {
           b <- c(0:3) * 2 * pi / 4 + pi/2
           # polygonGrob(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         diamondslim =  nstar(2, 0.6),
         star4 =  nstar(4, 0.5),
         pentagon =  {
           b <- c(0:4) * 2 * pi / 5 + pi/2
           # polygonGrob(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         star5 = nstar(5),
         star5slim = nstar(5, 0.3),
         star5fat =  nstar(5, .6),
         star5double =  ,
         hexagon =  {
           b <- c(0:4) * 2 * pi / 5 + pi/2
           # polygonGrob(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
           grid.polygon(((cos(b) + 1) / 2), ((sin(b) + 1) / 2), default.units = "native")
         },
         star6 = nstar(6),
         star6slim = nstar(6, 0.3),
         star6double = nstar(6, -0.8),
         star8 = nstar(8),
         star8slim  = nstar(8, 0.5),
         star8double = nstar(8, -0.8),
         if (point == 0) {
           grid.circle(x, y, r, default.units,
                   name, gp, draw, vp)
         }
         else {
           nstar(point, ratio)
         }
  )
}

# star5double <- function() { Treba najti način za neparne zvezde
#   nstar(5, -1.5)
# }

vp <- viewport(name = "ta_view")
pushViewport(vp)
nstar(5, -1)
grid.nstar(point = 6, ratio = sqrt(3))
grid.nstar(point = 3)
grid.nstar(shape = "triangle")
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

