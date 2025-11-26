if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

if (dir.exists("builds/library")) {
  .libPaths(c(normalizePath("builds/library"), .libPaths()))
}

# Note: Connection pool cleanup is handled automatically by the pool package's
# internal finalizers. Explicit cleanup in .Last can interfere with proper
# connection lifecycle management.
