## Test environments

* MacOS local install (Monterey 12.5.1), R 4.2.1 [0 NOTES, WARNINGS, ERRORS]
* Ubuntu latest (release) via GitHub actions, R 4.2.2 [0 NOTES, WARNINGS, ERRORS]
* Fedora Linux clang via R-hub, R-devel [3 NOTES, 0 WARNINGS, 0 ERRORS]

NOTE: Version contains large components (1.0.3.9000). This will be 
resolved by updating the package version just before CRAN submission.

NOTE: Examples with CPU (user + system) or elapsed time > 10s. Likely due to 
due to limited resources of remote instance.

NOTE: Skipping checking HTML validation: no command 'tidy' found. I can't 
control the dependencies on the remote instance.

* Windows Server 2022 via R-hub, R-oldrel [3 NOTES, 0 WARNINGS, 0 ERRORS (as previous)]
* Windows Server 2022 via win-builder, R-devel [2 NOTES, 0 WARNINGS, 0 ERRORS]

NOTE: Version contains large components (1.0.3.9000). This will be
resolved by updating the package version just before CRAN submission.

NOTE: Examples with CPU (user + system) or elapsed time > 10s. Likely due to 
due to limited resources of remote instance.

## revdepcheck results

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

