# setup for interpolate testing

interpolate <- function(new_df, model) {
  x <- stats::model.frame(model)[[deparse(model$terms[[3]])]]
  p <- polynom::polynomial(stats::coefficients(model))
  new_y <- as.list(new_df[[deparse(model$terms[[2]])]])
  new_x <- unlist(lapply(new_y, function(y) {
    roots <- solve(p, y)
    roots <- round(roots, digits = 8)
    root <- roots[which(Im(roots) == 0 & Re(roots) >= min(x) & Re(roots) <= max(x))]
    ifelse(identical(root, numeric(0)), NA, Re(root))
  }))
  new_x
}

x <- seq(0, 1, 0.2)
y <- -x ^ 2 + 2 * x + 2
plot(x, y)
df <- data.frame(x = x, y = y)
model <- lm(y ~ poly(x, 2, raw = TRUE), data = df)
new_df <- data.frame(y = c(0, 2, 2.5, 3, 3.5))

interpolate(new_df, model)




