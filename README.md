
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cubicle

Create clean, standardized project directories in seconds.

`cubicle` is an R package designed to initialize reproducible project
structures with a consistent layout, configurable defaults, and minimal
friction.

------------------------------------------------------------------------

## Installation

You can install `cubicle` with the following code.

``` r
devtools::install_github("bradyrippon/cubicle")
```

## Overview

The `cubicle` package helps you scaffold a new project from a predefined
template.

Instead of manually creating folders and files each time you start a
project, `cubicle` builds the structure for you automatically.

## Default Project Structure

A project created with `cubicle` looks like this:

``` text
proj-name/
├── code/
│   ├── admin/
│   │   ├── functions/
│   │   └── libs.R
├── data/
│   └── raw/
├── documents/
├── prints/
│   └── published/
├── records/
│   └── published/
├── zips/
├── figures/
├── reports/
├── tags/
├── .gitignore
├── notes.docx
└── proj.Rproj
```

## Usage

### Basic example

``` r
library(cubicle)

build_project(
  name = "my_project",
  path = "projects"
)
```

This creates a project at:

``` text
<root>/projects/my_project/
```

## Function Arguments

``` r
build_project(
  name,
  path,
  root = NULL,
  use_name = FALSE,
  load_project = FALSE,
  append = NULL
)
```

### Arguments

- `name`  
  Name of the project folder, used exactly as provided.

- `path`  
  Subdirectory within the root where the project will be created.

- `root`  
  Base directory for projects. If `NULL`, the configured default root is
  used.

- `use_name`  
  If `TRUE`, renames the `.Rproj` file to match the project name in
  snake_case.

- `load_project`  
  If `TRUE`, opens the new project in RStudio.

- `append`  
  Optional text to append to project files such as notes.

## Configuration

`cubicle` supports persistent configuration for default paths across
sessions.

These settings are stored using `rappdirs`.

### Set defaults

``` r
set_root("C:/Users/you/Documents/Projects")
set_template("C:/Users/you/Documents/Projects/cubicle-template")
```

### View current defaults

``` r
get_root()
get_template()
```

## Template Inspection

You can inspect the current template structure with:

``` r
show_template()
```

## Example Workflow

``` r
library(cubicle)

# set defaults once
set_root("C:/Users/brady/Documents/Projects")

# create a new project
build_project(
  name = "vept-analysis",
  path = "clinical",
  use_name = TRUE,
  load_project = TRUE
)
```

## Notes

- Empty directories in the template are preserved using placeholder
  files that are removed after project creation.
- `.Rproj.user` directories are excluded to avoid permission and copying
  issues.
- The package is intended to support repeatable and standardized project
  setup.

## Why use cubicle?

Standardized project structures make it easier to:

- keep work organized
- support reproducibility
- collaborate with others
- start new analyses quickly
- reduce setup mistakes

`cubicle` makes that process fast and consistent.

## License

MIT

## Author

Brady Rippon
