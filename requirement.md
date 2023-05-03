# List of all requirements needed to build Shiny modules

## Libraries
```
require(shiny)   # Necessity
require(ggplot2) # For better looking plots
require(dplyr)   # For easier data manipulation reading
```

## A module in Shiny
### What it needs
Shiny modules is an autonomous shiny app that help to contanerize the code.
Therefore they need:
- a `ui` to generate the layout of the interface
- a `server` to make all the computation
- a `demo` to test if everything works

It consumes a reactive variable (data)

### Organisation
Each modules could be stored in different files.
Each of them need documentation regarding what they do.
We can create a template for the different modules we want.

### Linting
It would be nice to have a standardized coding schema.
We could try to follow the [Tidy verse guidelines](https://style.tidyverse.org/files.html)
What could be done would be to use [`lintr`](https://lintr.r-lib.org/) as follow:

```
require(lintr)
lint(filename = "R/bad.R")
```

or use [`styler`](https://styler.r-lib.org/) as an addin directly in R
```
require(styler)
styler:::set_style_transformers()
```
<!-- Louis: I would prefer this one personnaly -->

## Some function we could add
- Download the graph
- Make it customisable (color, font, ...)
- Make it interactive (ggplot and plotly)
  - Add some hovering informations
