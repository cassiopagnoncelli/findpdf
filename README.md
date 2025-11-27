# dtools

Automatic probability distribution fitting for R. Identifies the best-fitting distribution for your data using particle swarm optimization.

## Installation

```r
# Install from source
install.packages("path/to/dtools_1.3.9.tar.gz", repos = NULL, type = "source")

# Or using devtools
devtools::install_github("cassiopagnoncelli/dtools")
```

## Usage

```r
library(dtools)

# Generate sample data
data <- rnorm(1000, mean = 5, sd = 2)

# Find best-fitting distribution
result <- findpdf(data)
print(result)

# Access fitted parameters
result$params$dnorm

# Evaluate PDF and CDF
result$pdf(5.0)
result$cdf(5.0)

# View ranking of all tested distributions
head(result$ranking)
```

## Features

- Automatic distribution detection (continuous and discrete)
- Particle swarm optimization for parameter estimation
- Comprehensive distribution catalog (normal, gamma, beta, Poisson, etc.)
- Optional exotic distributions support
- Statistical summaries and data analysis
- PDF/CDF function generation
- Distribution visualization

## Key Functions

- `findpdf()` - Identify best-fitting probability distributions
- `analyse()` - Analyze distribution properties
- `conform()` - Transform data to fit target distributions
- `cap()` - Apply bounded transformations
- `plot_distribution()` - Visualize fitted distributions

## Requirements

- R >= 4.4.0
- pso
- tibble
- dplyr
- ggplot2

## Development

```bash
make docs      # Generate documentation
make build     # Build package tarball
make install   # Install to local library
make test      # Run test suite
make check     # Run R CMD check
make lint      # Check code style
```

## License

MIT License. See [LICENSE](LICENSE) file.

## Author

Cassio Pagnoncelli ([ORCID: 0009-0000-7114-7008](https://orcid.org/0009-0000-7114-7008))
