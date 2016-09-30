library(ggplot2)
source("utils.R")
h <- list()
center.x <- as.integer()
center.y <- as.integer()
value <- as.integer()
i <- 1
for (n in 0:12) {
  for (k in 0:n) {
    center.x[i] <- k - 0.5*n
    center.y[i] <- -n * sin(pi/3)
    # value[i] = choose(n, k)
    x <- k - 0.5*n
    y <- -n * sin(pi/3)
    value[i] = choose(n, k)
    h[[i]] <- cbind(hexagon(c(center.x[i], center.y[i])), data.frame(n=n, k=k, value=value[i]))
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
  lapply(h, function(p) geom_polygon(data=p, aes(x=hx, y=hy, alpha=0, fill=value %% 2), color="black")) +
  annotate("text", x=center.x, y=center.y, label=value)
