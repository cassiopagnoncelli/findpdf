devtools::load_all()

x <- rnorm(1000000, 4.5, 1.2)

pdf <- findpdf(x)
pdf

pdf$best_fit
pdf$pdf
pdf$ranking
pdf$params
