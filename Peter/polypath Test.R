

outer <- list(x = c(0.1, 0.1, 0.9, 0.9), y = c(0.1, 0.9, 0.9, 0.1))
inner <- list(x = c(0.3, 0.3, 0.7, 0.7), y = c(0.3, 0.7, 0.7, 0.3))

windows(8, 8)
plot(NULL, xlim = c(0, 1), ylim = c(0, 1))
polygon(runif(24), runif(24), col = "green")
polygon(runif(24), runif(24), col = "blue")
polygon(runif(24), runif(24), col = "red")

polypath(x = c(outer$x, NA, inner$x), y = c(outer$y, NA, inner$y),
         col = scales::alpha("white", alpha = 0.6), rule = "evenodd",
         border = NA)



windows(8, 8)
plot(NULL, xlim = c(-1.0, 1.0), ylim = c(-1.0, 1.0), asp = 1)
polygon(runif(24, -1.2, 1.2), runif(24, -1.2, 1.2), col = "green")
polygon(runif(24, -1.2, 1.2), runif(24, -1.2, 1.2), col = "blue")
polygon(runif(24, -1.2, 1.2), runif(24, -1.2, 1.2), col = "red")

# circleSteps <- 360
# outer <- list(x = c(-1.2, -1.2, 1.2, 1.2), y = c(-1.2, 1.2, 1.2, -1.2))
# inner <- list(x = cos(seq(360/circleSteps, 360, by = 360/circleSteps/360)*2*pi/360),
#               y = sin(seq(360/circleSteps, 360, by = 360/circleSteps/360)*2*pi/360))
#
# polypath(x = c(outer$x, NA, inner$x), y = c(outer$y, NA, inner$y),
#          col = scales::alpha("white", alpha = 0.6), rule = "evenodd",
#          border = NA)

riemannMask(annotSouth = TRUE)



