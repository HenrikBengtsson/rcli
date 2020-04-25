![Life cycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

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

### For Bioconductor developers

To check a package for the currently installed version of Bioconductor, use:

```sh
$ R CMD check --as=bioconductor pkg_1.0.tar.gz
```

This will validate the package using the same "check" environment variables as the [Bioconductor servers](https://github.com/Bioconductor/BBS).

If you are submitting a [new package](https://bioconductor.org/developers/package-guidelines/) to Bioconductor, make sure that **[BiocCheck]** is installed.  Then run:

```sh
$ R CMD check --as=bioconductor::BiocCheck pkg_1.0.tar.gz
```

For available command-line options for the latter, see:

```sh
$ R CMD check --as=bioconductor::BiocCheck --help
```


### Customized checking

```sh
$ R CMD check --config=~/.R/my_bioc_check.dcf pkg_1.0.tar.gz
```

where `~/.R/my_bioc_check.dcf` may look like:

```
assert:
  getRversion() >= "4.0.0"
  utils::packageVersion("BiocVersion") >= "3.11"
  
options:
  --as=bioconductor
  --no-examples

env:
  _R_CHECK_CRAN_INCOMING_REMOTE_=false
```



[BiocCheck]: https://bioconductor.org/packages/BiocCheck/
[rcli]: https://github.com/HenrikBengtsson/rcli
