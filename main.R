library(ggplot2)
source("utils.R")
value_format <- function(v) {
  return(ifelse(v > 999, "---", v))  
}

# avoid "evaluation nested too deeply":
options(expressions=10000)
max_row = 29
h <- list()
center.x <- as.integer()
center.y <- as.integer()
value <- as.integer()
i <- 1
for (n in 0:max_row) {
  for (k in 0:n) {
    center.x[i] <- k - 0.5*n
    center.y[i] <- -n * sin(pi/3)
    x <- k - 0.5*n
    y <- -n * sin(pi/3)
    value[i] = choose(n, k)
    h[[i]] <- cbind(hexagon(c(center.x[i], center.y[i])), data.frame(n=n, k=k, value=value[i], fill=0))
    i <- i + 1
  }
}

g <- ggplot() + coord_fixed(1) +
  theme(
    legend.position="none",
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()  
  ) +
  scale_fill_gradient(low = "gray", high = "red")

plot <- function(polygons, show_values) {
  p <- g +
    lapply(polygons, function(p) geom_polygon(data=p, aes(x=hx, y=hy, alpha=0, fill=fill), color="black"))
  if (show_values)
    p <- p + annotate("text", x=center.x, y=center.y, label=value_format(value))
  return(p)
}
