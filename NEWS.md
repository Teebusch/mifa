# mifa 0.2.1

* Update cross-references to mice package in documentation to comply with new
  CRAN checks
* Update example code and links in README
* Remove unnecessary "LazyData" key from DESCRIPTION to remove CRAN check Note

# mifa 0.2.0

* Major code refactoring for readability and maintainability
* Vastly improved function signatures (breaking changes to v0.1.0)
* Added argument checks for all user-facing functions
* Add `cov_vars` argument for selecting variables to use for for imputations 
  but not for covariance matrix 
* Complete control over `mice()` via `...` arguments
* Better function returns, e.g. main function now returns an `S3` object of 
  class `mifa` which has nice `summary()` and `print()` generics
* Much improved documentation and examples 
* Added a `README.md` with installation and usage instructions
* Added a `{pkgdown}` site at <https://teebusch.github.io/mifa/>
* Added a `NEWS.md` file to track changes to the package.
* Remove unused dependencies
* Added continuous integration and unit tests


# mifa 0.1.0

* Initial release, accompanying the paper in Behavioral Research Methods 
  [(link)](https://doi.org/10.3758/s13428-017-1013-4)
* mifa 0.1.0 was originally released at <https://github.com/vahidnassiri/mifa/>
  but this repository appears to have been abandoned
