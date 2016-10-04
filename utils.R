
polar_point <- function(r, theta) {
  x <- r * cos(theta)
  y <- r * sin(theta)
  return(data.frame(x=x, y=y))
}

polar_point2 <- function(r, theta) {
  x <- r * cos(theta)
  y <- r * sin(theta)
  return(t(list(x, y)))
}

base_hexagon <- function(center, r) {
  hx <- r * cos(0:5 * pi / 3 + pi/6) + center[1]
  hy <- r * sin(0:5 * pi / 3 + pi/6) + center[2]
  return(data.frame(hx=hx, hy=hy))
}

hexagon <- function(center) {
  return(base_hexagon(center, 0.5/sin(pi/3)))
}

# Fill functions. All must return integer.
mod2 <- function(v, n) ifelse(v %% 2 == 0, 1, 0)
mod3 <- function(v, n) ifelse(v %% 3 == 0, 1, 0)
modn <- function(v, n) {ifelse(n == 0, 0, ifelse(v %% n == 0, 1, 0))}
prime <- function(v, n) ifelse(n == 0, 0, ifelse(isPrime(n) && modn(v, n), 1, 0))
value <- function(v, n) v
