---
name: CRAN release
about: This is a checklist for releasing a new version of `galah` to CRAN
title: CRAN release version [VERSION]
labels: ''
assignees: ''

---

## Documentation
- [ ] Documentation has been built with `roxygen::roxygenise()`
- [ ] `pkgdown` site has been built locally with `pkgdown::build_site()` and all the pages are rendering as expected
- [ ] NEWS.md has been updated with all the changes since the last release, including links to issue numbers

## Testing
- [ ] All automated tests are passing locally with `devtools::test()` (including slow skipped tests)
- [ ] All functional tests documented [here](https://confluence.csiro.au/display/ALASD/galah+and+ALA4R+functional+tests) have been performed, ideally by someone other than the developer
- [ ] Code has been merged into the `master` branch and pushed to Git
- [ ] Tests are passing on the `master` branch on [travis](https://travis-ci.com/github/AtlasOfLivingAustralia/galah) 

## External checks
- [ ] `R CMD build galah` and `R CMD check galah_{version}.tar.gz` run with no issues
- [ ] Package checked using `rhub::check_for_cran()` 
- [ ] `rhub` results (emailed to maintainer) have no errors or warnings
- [ ] Package zipfile has been submitted to [winbuilder](https://win-builder.r-project.org/upload.aspx)
- [ ] winbuilder results (emailed to maintainer) have no errors or warnings

## Change Advisory Board (CAB) submission
- [ ] Change request submitted to CAB
- [ ] Change request approved by CAB
