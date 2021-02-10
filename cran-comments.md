## Resubmission

This is a resubmission. In this version I have:

* Put 'ggplot2' and 'GraphPad Prism' in single quotes in the Title and 
  Description fields.
* Added missing \value sections to the .Rd files for exported methods 
  to explain what each function returns.
* Removed set_seed() from the preview_theme() function.

* Removed the separate LICENSE file and fixed the License field.
* Fixed the doi link in wings.Rd to use \doi{}.

## Test environments

* MacOS local install (Big Sur 11.1), R 4.0.3
* MacOS latest (release) via GitHub actions, R 4.0.3
* Ubuntu 20.04 (release) via GitHub actions, R 4.0.3
* Ubuntu 20.04 (devel) via GitHub actions, R 4.0.3
* Windows latest (release) via GitHub actions, R 4.0.3
* Windows Server 2008 R2 SP1 via R-hub, R-devel
* Windows via win-builder, R-devel

## R CMD check results

There were no ERRORs or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  New submission

This is my first submission so this note is unavoidable.

* Possibly mis-spelled words in DESCRIPTION: GraphPad, ggplots

These words are not mis-spelled.

## Downstream dependencies

There are currently no downstream dependencies for this package.
