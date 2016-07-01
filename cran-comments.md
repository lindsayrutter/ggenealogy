## Test environments
* local OS X install, R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking data for non-ASCII characters ... NOTE
Note: found 2356 marked UTF-8 strings

This note comes from the new example dataset that has just been added to this version of the package. This example dataset (which is located at data/statGeneal.rda) comes from a database that contains names of academic mathematicians and universities worldwide, many of which contain characters outside of ASCII format. In order to preserve the characters in their original format (instead of converting them to unaccented English characters), we did not convert them to ASCII. Instead, we converted them into UTF-8 encodings in order to render them portable.

## Downstream dependencies
There are currently no downstream dependencies for this package.

