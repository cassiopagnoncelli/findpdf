devtools::load_all()

x <- rnorm(1000000, 4.5, 1.2) + rnorm(2000000, .8, 1.4)

plot_distribution(x, bins = "Sturges", groups = c(1, 2, 4))
