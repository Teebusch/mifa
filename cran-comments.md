## Resubmission
This is a resubmission. These changes have been made:

* Wrap example in `\donttest{}` instead of `\dontrun{}`. 
  This examples take longer than 5 seconds to run (locally and on win-builder), 
  so they should be skipped on CMD CHECK.
* Reduce number of bootstrap samples in example for `mifa_ci_boot()` from 50 
  to 10 to decrease running time.
* Wrap all 3 examples in `if(requireNamespace("psych")){}` because `psych` is
  only a suggested package.

## Test environments
* GitHub Actions (ubuntu-20.04): release, devel, oldrel
* GitHub Actions (windows): release, devel, oldrel
* Github Actions (macOS): release, devel, oldrel

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
