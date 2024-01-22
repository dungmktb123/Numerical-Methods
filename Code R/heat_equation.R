dx <- 0.2
xgrid <- setup.grid.1D(x.up = -100, x.down = 100, dx.1 = dx)
x <- xgrid$x.mid
N <- xgrid$N
lam <- 0.05
uini <- exp(-lam*x^2)
vini <- rep(0, N)
yini <- c(uini, vini)
times <- seq (from = 0, to = 50, by = 1)
wave <- function (t, y, parms) {
  u <- y[1:N]
  v <- y[(N+1):(2*N)]
  du <- v
  dv <- tran.1D(C = u, C.up = 0, C.down = 0, D = 1,
                dx = xgrid)$dC
  return(list(c(du, dv)))
}
out <- ode.1D(func = wave, y = yini, times = times,
              parms = NULL, method = "adams",
              dimens = N, names = c("u", "v"))
u <- subset(out, which = "u")
analytic <- function (t, x)
  0.5 * (exp(-lam * (x+1*t)^2 ) +exp(-lam * (x-1*t)^2) )
OutAna <- outer(times, x, FUN = analytic)
max(abs(u - OutAna))
outtime <- seq(from = 0, to = 50, by = 10)
matplot.1D(out, which = "u", subset = time %in% outtime,
           grid = x, xlab = "x", ylab = "u", type = "l",
           lwd = 2, xlim = c(-50, 50),
           col = c("black", rep("darkgrey", 5)))
legend("topright", lty = 1:6, lwd = 2,
       col = c("black", rep("darkgrey", 5)),
       title = "t = ", legend = outtime)
