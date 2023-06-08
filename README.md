# KIDS23-Team13: Reusable R Shiny modules for common plots and data types

- [Shiny modules](#what-is-a-shiny-module)
- [Aim of the package](#aim-of-this-package)
- [Organisation and contribution](#organisation)

## What is a shiny module

Shiny modules is an autonomous shiny app that help to contanerize the code.
Therefore they need:

- a `ui` to generate the layout of the interface
- a `server` to make all the computation
- a `demo` to test if everything works

It consumes a reactive variable (data)

### Why shiny modules

The magic of modules comes because these functions are constructed in a special way that creates a “namespace”. So far, when writing an app, the names (ids) of the controls are global: all parts of your server function can see all parts of your UI. Modules give you the ability to create controls that can only be seen from within the module. This is called a namespace because it creates “spaces” of “names” that are isolated from the rest of the app.

Shiny modules have two big advantages. Firstly, namespacing makes it easier to understand how your app works because you can write, analyse, and test individual components in isolation. Secondly, because modules are functions they help you reuse code; anything you can do with a function, you can do with a module.

For more informations, follow this [link](https://mastering-shiny.org/scaling-modules.html)

## Aim of this package

This package provide different modules that you can use as they are or that you can easily reusable through your apps.
The aim is to provide simple yet effective shiny sub-unit to factorise your code and therefore improve the reading quality anf reusability.

The following modules are available:

- dataImport: import one dataframe from csv, xlsx, xls or rda
- selectVar: select a column based on a dataframe and filtering

## Organisation

For information about the organisation of this project please find the [organisation.md](/documentation/organisation.md) file.

### Contribution

Contribution guidelines can be found in the [CONTRIBUTING.md](/documentation/CONTRIBUTING.md) file.

### Project board

A [project board](https://github.com/orgs/stjude-biohackathon/projects/7/views/1) is used to follow the works in progress.
