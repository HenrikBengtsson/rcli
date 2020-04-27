![Life cycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

The **[rcli]** package boosts `R CMD check` with extra options.  For example, `R CMD check --config=check.dcf pkg_1.0.tar.gz` checks the package tarball with additional command-line options and environment variables as given by the 'check.dcf' file.  Another example is `R CMD check --as=bioconductor pkg_1.0.tar.gz`, which checks the package according to Bioconductor settings.


## One-time setup

After installing the **rcli** package (see below), call

```r
> rcli::install()
Backed up R startup file: '~/.Rprofile' (316 bytes) -> '~/.Rprofile.bak.20200425-210609' (316 bytes)
Added 'rcli::r_cmd_call()' to already existing R startup file: '~/.Rprofile'
Validated that 'R CMD check --as=<style>' works
```

to activate the **rcli** addons.  To see it for yourself, call the following from the command line:

```sh
$ R CMD check --as=rcli --hello
* using --as=rcli
Hello world!
$ 
```


## Usage

### For CRAN developers

When submitting to CRAN, you should validate your package with `R CMD check --as-cran`.  For the completeness of this package, an alias for that is:

```sh
$ R CMD check --as=cran pkg_1.0.tar.gz
```

If you are offline, on a slow or an unstable internet connection, or for other reasons do not want to send out online queries to the CRAN servers, you can run these checks in an "offline" mode by:

```sh
$ R CMD check --as=cran --offline pkg_1.0.tar.gz
```

This will _as far as possible_ try to skip the tests that require data to be pulled down from the CRAN servers.  



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
$ R CMD check --config=~/.R/my_bioc-3.11_check.dcf pkg_1.0.tar.gz
```

where `~/.R/my_bioc-3.11_check.dcf` may look like:

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


## Known limitations

The above `R CMD` extensions work because **rcli** injects itself, during the R startup process, in the first step of the `R` -> `R CMD` -> `R CMD {command}` pipeline.  Because of this, none of the above will work if we call it as:

```sh
$ R --vanilla CMD check ...
$ R --no-init-file CMD check ...
```

If attempted, we will get something like:

```sh
$ R --vanilla CMD check --as=rcli --hello
Warning: unknown option ‘--as=rcli’
Warning: unknown option ‘--hello’
Error: no packages were specified
```

[BiocCheck]: https://bioconductor.org/packages/BiocCheck/
[rcli]: https://github.com/HenrikBengtsson/rcli
