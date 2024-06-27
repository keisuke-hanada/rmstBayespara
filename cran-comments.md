## Check environment
We check local and win-builder.


## R CMD check results
0 errors | 0 warnings | 4 notes

note 1: checking installed package size installed size is  6.0Mb. sub-directories of 1Mb or more: libs 5.7Mb

- This package uses stan and requires a large amount of memory to compile.

note 2: checking for GNU extensions in Makefiles. GNU make is a SystemRequirements

- GNU make was used to create packages with "rstantools".

note 3: checking CRAN incoming feasibility. Maintainer: 'Keisuke Hanada <keisuke.hanada.87@gmail.com>'.

- Maintainer is specified.

note 4: checking examples [30s]. Examples with CPU (user + system) or elapsed time > 10s

- Stan takes some time to execute, so even an example requires more than 10 seconds of execution time.

## revdepcheck results

There are currently no downstream dependencies for this package.
