# To contribute to this package

Those contribution guidelines are adapted from the [github documentation](https://github.com/github/docs)

Thank you for investing your time in contributing to our project! :sparkles:.

Read our [Code of Conduct](/documentation/CODE_OF_CONDUCT.md) to keep our community approachable and respectable.

In this guide you will get an overview of the contribution workflow from opening an issue, creating a PR, reviewing, and merging the PR.
But also information about the organisation of the project, the tests and documentation needed for each contribution.

- [New contributor guide](#new-contributor-guide)
    - [Getting started](#getting-started)
    - [Issues](#issues)
        - [Create a new issue](#create-new-issue)
        - [Solve an issue](#solve-an-issue)
    - [Make changes](#make-changes)
        - [Make changes in the UI](#make-changes-in-the-ui)
        - [Make changes in a codespace](#make-changes-in-a-codespace)
        - [Make changes locally](#make-changes-locally)
    - [Commit your update](#commit-your-update)
    - [Pull request](#pull-request)
    - [Your PR is merged](#your-pr-is-merged)
- [Package organisation](#package-organisation)
- [R functions conventions](#r-functions-conventions)
    - [Documentation](#documentation)
    - [Writing](#writing)
    - [Testing](#testing)
- [Package building](#package-building)
    - [Requirements](#requirements)



## New contributor guide

To get an overview of the project, read the [README](README.md). Here are some resources to help you get started with open source contributions:

- [Finding ways to contribute to open source on GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/finding-ways-to-contribute-to-open-source-on-github)
- [Set up Git](https://docs.github.com/en/get-started/quickstart/set-up-git)
- [GitHub flow](https://docs.github.com/en/get-started/quickstart/github-flow)
- [Collaborating with pull requests](https://docs.github.com/en/github/collaborating-with-pull-requests)


### Getting started

Check to see what [types of contributions](/documentation/types-of-contributions.md) we accept before making changes. Some of them don't even require writing a single line of code :sparkles:.

### Issues

#### Create a new issue

If you spot a problem with the package, [search if an issue already exists](https://github.com/stjude-biohackathon/KIDS23-Team13/issues). If a related issue doesn't exist, you can open a new issue.

#### Solve an issue

Scan through our [existing issues](https://github.com/stjude-biohackathon/KIDS23-Team13/issues) to find one that interests you. You can narrow down the search using `labels` as filters. As a general rule, we don’t assign issues to anyone. If you find an issue to work on, you are welcome to open a PR with a fix.

### Make Changes

#### Make changes in the UI

Click **Make a contribution** at the bottom of any docs page to make small changes such as a typo, sentence fix, or a broken link. This takes you to the file where you can make your changes and [create a pull request](#pull-request) for a review.

#### Make changes in a codespace

For more information about using a codespace for working on GitHub documentation, see "[Working in a codespace](https://github.com/github/docs/blob/main/contributing/codespace.md)."

#### Make changes locally

1. Fork the repository.
    [Fork the repo](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo#fork-an-example-repository) so that you can make your changes without affecting the original project until you're ready to merge them.
2. Create a working branch and start with your changes!
    Use a `branch name` adapted (can be the name of the issue: `issue-30_FixHistogram`)
    ```bash
    git checkout -b branch_name
    ```

### Commit your update

Commit the changes once you are happy with them.

### Pull Request

When you're finished with the changes, create a pull request, also known as a PR.
- Fill the "Ready for review" template so that we can review your PR. This template helps reviewers understand your changes as well as the purpose of your pull request.
- Don't forget to [link PR to issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue) if you are solving one.
- Enable the checkbox to [allow maintainer edits](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/allowing-changes-to-a-pull-request-branch-created-from-a-fork) so the branch can be updated for a merge.
Once you submit your PR, a team member will review your proposal. We may ask questions or request additional information.
- We may ask for changes to be made before a PR can be merged, either using [suggested changes](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/incorporating-feedback-in-your-pull-request) or pull request comments. You can apply suggested changes directly through the UI. You can make any other changes in your fork, then commit them to your branch.
- As you update your PR and apply changes, mark each conversation as [resolved](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/commenting-on-a-pull-request#resolving-conversations).
- If you run into any merge issues, checkout this [git tutorial](https://github.com/skills/resolve-merge-conflicts) to help you resolve merge conflicts and other issues.

### Your PR is merged!

Congratulations :tada::tada: The GitHub team thanks you :sparkles:.

Once your PR is merged, your contributions will be publicly visible on the [contributors page](https://github.com/stjude-biohackathon/KIDS23-Team13/graphs/contributors).

Now that you are part of our community, see how else you can [contribute to the docs](/documentation/types-of-contributions.md).

## Package organisation

R package organisation is quite straigth forward:

- All R functions are stored in the `R` folder.
- All documentation are stored in the `man` folder.
- Data samples are stored in the `data` folder.
- The `tests` folder contains the unnitest runned during compilation
- Some `vignettes` are also available in their own folder to explain more the codes.

## R functions conventions

### Documentation

Each function are preceded by ROxygen comment to render the documentation of each function.
To transfer those ROxygen comment to Rd files in the `man` folder, run:

```R
devtools::document()
```

### Writing

One good guidelines that could be followed is the one from the [**TidyVerse**](https://style.tidyverse.org/).
It explains how to name files, functions, variable, for analyses and packages.
It also add information on how to nicely write the code: space, indentations, ...

All those guidelines can be automatically detected and applied to a package with [**styler**](https://styler.r-lib.org/) package.

### Testing

One objective would be to add tests for each functions and for each conditions.
For that we use the package [**test_that**](https://testthat.r-lib.org/).
For a given function a linked test file can be created in the `test/testthat/` folder with

```R
use_test("function_name")
```

When the test is added inside the file it will be ran during compilation or can be done with:

```R
devtools::test()   
```

To see the coverage of the generated code you can use:

```R
library(covr)

# If run with no arguments implicitly calls `package_coverage()`
report()
```

## Package building

### Requirements

To build the package [Rtools](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html) will to be installed.

The package will need to pass the following test:

```r
devtools::build()
devtools::check()

BiocCheck::BiocCheckGitClone()
BiocCheck::BiocCheck('BioShinyModules'=TRUE)
```
