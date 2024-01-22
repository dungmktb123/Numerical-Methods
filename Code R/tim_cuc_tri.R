bisectmax <- function(f, a, b, tol = 1e-6) {
  phi <- (sqrt(5)-1)/2
  a.star <- b - phi * abs(b - a)
  b.star <- a.star + phi * abs(b - a.star)
  while (abs(b - a) > tol) {
    if(f(a.star) < f(b.star))
    {
      b <- b.star
      b.star <- a.star
      a.star <- b - phi*abs(b-a)
    }
    else {
      a <- a.star
      a.star <- b.star
      b.star <- a + phi*abs(b-a)
    }
  }
  return((a + b) / 2)
}

ff <- function(x) {
  return(-x^2 + x + 2)
}

x_values <- seq(-2, 2, length.out = 100)

# Evaluate the ff function for each x value
y_values <- ff(x_values)

# Plot the function
plot(x_values, y_values, type = "l", col = "blue", lwd = 2,
     main = "Graph of ff(x) = -x^2 + x + 2",
     xlab = "x", ylab = "f(x)")

# Add a grid
grid()

# Add a legend
legend("topright", legend = "ff(x) = -x^2 + x + 2", col = "blue", lty = 1, lwd = 2)

# Call the bisectmax function and display the extremum
extremum <- bisectmax(ff, -2, 2, tol = 1e-15)
points(extremum, ff(extremum), col = "red", pch = 16)
extremum
