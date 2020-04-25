# rcli: R Command-Line Interface Booster

## One-time Setup

After installing the **[rcli]** package (see below), add the following to the end of your `~/.Rprofile` startup file:

```sh
if (nzchar(Sys.getenv("R_CMD")) && require("rcli", quietly=TRUE)) {
  rcli::r_cmd_call()
}
```

Validate that you get the following output:

```sh
$ R CMD check --as=rcli-test
* using --as=rcli-test
```


## Usage

### For CRAN developers

```sh
$ R CMD check --as=cran pkg_1.0.tar.gz
$ R CMD check --as=cran::newbie pkg_1.0.tar.gz
```


### For Bioconductor developers

```sh
$ R CMD check --as=bioconductor pkg_1.0.tar.gz
$ R CMD check --as=bioconductor::newbie pkg_1.0.tar.gz
$ R CMD check --as=bioconductor::newbie --help
```


### Customized checking

```sh
$ R CMD check --config=~/R/my_check.dcf pkg_1.0.tar.gz
```


[rcli]: https://github.com/HenrikBengtsson/rcli

## Installation
R package rcli is only available via [GitHub](https://github.com/HenrikBengtsson/rcli) and can be installed in R as:
```r
remotes::install_github("HenrikBengtsson/rcli")
```




## Contributions

This Git repository uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).  The [`develop`](https://github.com/HenrikBengtsson/rcli/tree/develop) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/HenrikBengtsson/rcli) branch contains the code of the latest release.

Contributing to this package is easy.  Just send a [pull request](https://help.github.com/articles/using-pull-requests/).  When you send your PR, make sure `develop` is the destination branch on the [rcli repository](https://github.com/HenrikBengtsson/rcli).  Your PR should pass `R CMD check --as-cran`, which will also be checked by  and  when the PR is submitted.


## Software status

| Resource      | GitHub        | GitHub Actions      | Travis CI       | AppVeyor CI      |
| ------------- | ------------------- | ------------------- | --------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Multiple_          | _Linux & macOS_ | _Windows_        |
| R CMD check   |  |        |    |  |
| Test coverage |                     |                     |      |                  |
