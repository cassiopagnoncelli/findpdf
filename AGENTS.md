# Repository Guidelines

## Project Structure & Module Organization
- `R/` contains the core functions (`data-summary.R` for dataset metadata, `infer-distribution.R` orchestrating inference, `pf-database.R` defining the distribution catalog, and `dtools-internal.R` with saved state used by inference).
- `tests/testthat.R` wires the testthat harness (edition 3); add specs under `tests/testthat/test_*.R`.
- `builds/` holds built tarballs and the local install library (`builds/library`) that `.Rprofile` prepends to `.libPaths()`; `man/` and `NAMESPACE` are generated via roxygen2; `rd/` and `README.md` carry supporting resources.
- Tooling/config lives in `.lintr`, `.Rbuildignore`, `.Rprofile`, and `.Renviron` (compiler flags).

## Environment & Dependencies
- Requires R >= 4.4.0. `.Renviron` pins LLVM/clang with OpenMP flags from `/opt/homebrew/opt/llvm` and `libomp`; keep this when compiling.
- Runtime inference code uses base R plus `pso` and `GenSA`; ensure these are installed/declared when touching inference logic.
- Development helpers: `devtools` for docs/tests/check, `testthat` (edition 3) for unit tests, `lintr`/`styler` for linting/formatting, and optional `withr`, `cyclocomp`, `MASS`, `knitr` from `Suggests`.
- `.Rprofile` will activate `renv` if present and prepends `builds/library` to `.libPaths()` to use locally installed packages.

## Build, Test, and Development Commands
- `make docs` runs `devtools::document()` to refresh `man/` and `NAMESPACE`.
- `make build` (runs `docs`) builds a tarball and moves it into `builds/`; `make install` installs the latest tarball into `builds/library` and prints `.libPaths()`.
- `make check` executes `R CMD check .`; `make test` (alias `make tests`) runs `devtools::test()` via the testthat harness.
- `make lint` uses the `.lintr` config; `make style` runs `styler::style_dir('.')`; `make clean` removes build artifacts; `make stats` reports LOC and change stats; `make uninstall` removes the installed package.

## Coding Style & Naming Conventions
- Use 2-space indents and keep lines within 120 characters (per `.lintr`); follow the existing lowerCamelCase naming (e.g., `dataSummary`, `dtools`).
- Favor vectorized base-R code; validate inputs early with clear `stop()` messages when adding arguments.
- When adding exported functions, include roxygen2 blocks so `make docs` regenerates docs/NAMESPACE; keep examples runnable and minimal.
- Avoid new global assignments unless updating the pf database (which intentionally populates globals).

## Testing Guidelines
- Add tests under `tests/testthat/test_*.R` using testthat (edition 3).
- Cover edge cases: NA/NaN filtering, discrete vs. continuous detection, domain inference, and optimizer behavior for candidate distributions.
- Run `make test` for quick checks and `make check` before publishing broader changes.

## Commit & Pull Request Guidelines
- Commit messages: concise, imperative summaries (e.g., “Add chi-squared bounds check”); keep bodies short unless extra context is needed.
- PRs should explain intent, list commands executed (tests/check/lint/style), link related issues, and flag API or behavioral changes.
- Include reproducible examples; screenshots only when visual outputs change.
