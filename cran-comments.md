## Test environments

* MacOS local install (Sonoma 14.2.1), R 4.3.2 [0 NOTES, WARNINGS, ERRORS]
* Ubuntu latest (release) via GitHub actions, R 4.3.3 [0 NOTES, WARNINGS, ERRORS]
* Ubuntu latest (devel) via GitHub actions, R 4.3.3 [0 NOTES, WARNINGS, ERRORS]
* Ubuntu latest (oldrel) via GitHub actions, R 4.2.3 [0 NOTES, WARNINGS, ERRORS]
* Windows latest (release) via GitHub actions, R 4.3.3 [0 NOTES, WARNINGS, ERRORS]
* MacOS latest (release) via GitHub actions, R 4.3.3 [0 NOTES, WARNINGS, ERRORS]
* Fedora Linux clang via R-hub, R-devel [2 NOTES, 0 WARNINGS, 0 ERRORS]

NOTE: Examples with CPU (user + system) or elapsed time > 5s. Likely due to 
due to limited resources of remote instance.

NOTE: Skipping checking HTML validation: no command 'tidy' found. I can't 
control the dependencies on the remote instance.

* Windows Server 2022 via R-hub, R-oldrel [3 NOTES, 0 WARNINGS, 0 ERRORS (as previous)]

NOTE: Examples with CPU (user + system) or elapsed time > 5s. Likely due to 
due to limited resources of remote instance.

NOTE: Found the following files/directories: ''NULL''. RHub bug according to
https://github.com/r-hub/rhub/issues/560

NOTE: Found the following files/directories: 'lastMiKTeXException'.
RHub bug according to https://github.com/r-hub/rhub/issues/503

* Windows Server 2022 via win-builder, R-devel [0 NOTES, 0 WARNINGS, 0 ERRORS]

## revdepcheck results

We checked 5 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

