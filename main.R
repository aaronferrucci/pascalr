library(ggplot2)
source("utils.R")
h <- list()
for (n in 0:9) {
  for (k in 0:n) {
    x <- k - 0.5*n
    y <- -n * sin(pi/3)
    value = choose(n, k)
    h[[length(h) + 1]] <- cbind(hexagon(c(x, y)), data.frame(n=n, k=k, value=value, color=value %% 2))
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
  lapply(h, function(p) geom_polygon(data=p, aes(x=x, y=y, alpha=0.25, fill=color), color="black"))
# g + lapply(h, function(p) geom_path(data=p, aes(x=x, y=y, alpha=0.25), color="black"))
# h <- lapply(h, function(df) df$color <- value %% 3, df)
# colors <- lapply(h, function(df) df$color)
