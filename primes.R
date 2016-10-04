library(numbers)

source("main.R")
moddy <- function(v, n) ifelse(n == 0, 0, ifelse(modn(v, n), ifelse(isPrime(n), 2, 1), 0))
h2 <- lapply(h, function(p) { p$fill <- moddy(p$value, p$n); return(p)})
the_plot <- plot(h2, F) +
  annotate("text",
    x=seq(from=0, by=-0.5, length.out=max_row + 1),
    y=seq(from=0, by=-sin(pi/3), length.out=max_row + 1),
    label=sapply(seq(0, max_row), function(x) ifelse(x == 0, "", ifelse(isPrime(x), x, "")))
  )
print(the_plot)