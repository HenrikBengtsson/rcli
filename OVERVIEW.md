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
$ 
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
