# Shiny Apps with renv

This project uses [renv](https://rstudio.github.io/renv/) to ensure reproducible package versions across machines.

## Getting Started

1. Clone or copy the Apps repository.
2. Open R or RStudio in the project folder.
3. Run the following commands:
  
 
# Activate the renv environment
renv::activate()

# Install the packages listed in renv.lock
renv::restore()


# if installed new library needed
# commit to renv lock file
renv::snapshot()

# of on another machine
renv::restore() # reads the new lock file and installs

