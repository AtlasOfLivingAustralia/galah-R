---
name: CRAN release
about: This is a checklist for releasing a new version of `galah` to CRAN
title: CRAN release version [VERSION]
labels: ''
assignees: ''

---

## Documentation
- [ ] Vignettes have been updated by opening the `vignettes/precompile.R` file and running the script
- [ ] Documentation has been built with `roxygen2md::roxygen2md()`
- [ ] `_pkgdown.yml` has been updated with any new/updated changes
- [ ] `pkgdown` site has been built locally with `pkgdown::build_site()` and all the pages are rendering as expected
- [ ] NEWS.md has been updated with all the changes since the last release, including links to issue numbers
- [ ] Version number has been increased in the `DESCRIPTION` in [semantic versioning style](https://semver.org/)

## Testing
- [ ] Delete all files from the `tests/fixtures/` folder (except any international atlas fixtures not working because an API system is down)
- [ ] Delete all files from the `tests/testhat/_snaps/` folder (these need to be deleted each time before running `devtools::test()`)
- [ ] Restart R (Ctrl/Cmd + Shift + F10)
- [ ] All automated tests are passing locally with `devtools::test()` (including slow skipped tests)
- [ ] All functional tests documented [here](https://confluence.csiro.au/display/ALASD/galah+and+ALA4R+functional+tests) have been performed, ideally by someone other than the developer
- [ ] Code has been merged into the `master` branch and pushed to Git (including re-generated fixtures)
- [ ] Tests are passing on the `master` branch on [travis](https://travis-ci.com/github/AtlasOfLivingAustralia/galah) 

## External checks
- [ ] `R CMD build galah` and `R CMD check --as-cran --no-manual galah_{version}.tar.gz` run with no issues
- [ ] Package checked using `rhub::check_for_cran()` 
- [ ] `rhub` results (emailed to maintainer) have no errors or warnings
- [ ] Package zipfile has been submitted to [winbuilder](https://win-builder.r-project.org/upload.aspx)
- [ ] winbuilder results (emailed to maintainer) have no errors or warnings

## Change Advisory Board (CAB) submission
- [ ] Change request submitted to CAB in [JIRA](https://jira-sd.csiro.au/projects/ACRR/queues/custom/367)
- [ ] Change request approved by CAB

## Submit to CRAN
- [ ] Upload package to CRAN here: https://cran.r-project.org/submit.html
- [ ] Click on link in email from CRAN and confirm submission
- [ ] Fix any errors from CRAN and repeat all steps :(
- [ ] Celebrate!
