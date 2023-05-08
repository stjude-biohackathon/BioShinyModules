# List of all requirements needed to build Shiny modules

## Libraries

```
library(shiny)   # Necessity
library(ggplot2) # For better looking plots
library(dplyr)   # For easier data manipulation reading
library(devtools)# For easier development workflow
```

## A module in Shiny

### What it needs

Shiny modules is an autonomous shiny app that help to contanerize the code.
Therefore they need:

- a `ui` to generate the layout of the interface
- a `server` to make all the computation
- a `demo` to test if everything works

It consumes a reactive variable (data)

### Why Shiny modules

The magic of modules comes because these functions are constructed in a special way that creates a “namespace”. So far, when writing an app, the names (ids) of the controls are global: all parts of your server function can see all parts of your UI. Modules give you the ability to create controls that can only be seen from within the module. This is called a namespace because it creates “spaces” of “names” that are isolated from the rest of the app.

Shiny modules have two big advantages. Firstly, namespacing makes it easier to understand how your app works because you can write, analyse, and test individual components in isolation. Secondly, because modules are functions they help you reuse code; anything you can do with a function, you can do with a module.

For more informations, follow this [link](https://mastering-shiny.org/scaling-modules.html)

### Organisation

This [tutorial](https://shiny.rstudio.com/articles/modules.html) is really nice and explain a lots of interesting thing.
Each modules could be stored in different files.
We can create a template for the different modules we want.

### Documentation

Each of them need documentation regarding what they do.
For everyone to contribute to their own module documentation, we can go with the recommendation stated in the [R package guidelines](https://r-pkgs.org/man.html#sec-man-workflow), and add roxygen comments at the start of each modules.

### Linting

It would be nice to have a standardized coding schema.
We could try to follow the [Tidy verse guidelines](https://style.tidyverse.org/files.html)
What could be done would be to use [`lintr`](https://lintr.r-lib.org/) as follow:

``` R
require(lintr)
lint(filename = "R/bad.R")
```

or use [`styler`](https://styler.r-lib.org/) as an addin directly in R

``` R
require(styler)
styler:::set_style_transformers()
```
<!-- Louis: I would prefer this one personnaly -->

## Some function we could add

- Download the graph
- Make it customisable (color, font, ...)
- Make it interactive (ggplot and plotly)
  - Add some hovering informations
