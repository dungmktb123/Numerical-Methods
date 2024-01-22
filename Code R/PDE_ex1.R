# Loading package
library(deSolve)

# Parameters
D <- 0.3 # m2/day diffusion rate
r <- 0.03  # /day net growth rate
delx <- 1 # m thickness of boxes
numboxes <- 60 
# distance of boxes on plant, m, 1 m intervals
Distance <- seq(from = 0.5, by = delx, length.out = numboxes)
times <-seq(0, 200, by = 1)

# Initial conditions
APHIDS <- rep(0, times = numboxes)
APHIDS[30] <- 1
APHIDS[40] <- 1
state <- c(APHIDS = APHIDS)

# Equation
Aphid <- function(t, APHIDS, parameters) {
  deltax <- c (0.5, rep(1, numboxes - 1), 0.5); 
  Flux <- -D * diff(c(0, APHIDS, 0)) / deltax;
  dAPHIDS <- -diff(Flux) / delx + APHIDS * r;
  list(dAPHIDS )}

# Run
out <- ode.1D(state, times, Aphid, parms = 0, nspec = 1)
#head(out)

# Drawing
image(out, method = "filled.contour", grid = Distance)
matplot.1D(out, grid = Distance, type= "l")
#plot.1D(out, grid = Distance, type= "l")
head(out)