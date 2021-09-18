# viscomplexr 1.1.1

* Default number of processor cores to be used by the functions 
  *phasePortrait* and *phasePortraitBw* was reduced to one core less than 
  available
* Removed the LazyData specification from the DESCRIPTION as the package
  does not comprise any data. This avoids a NOTE in the CRAN package check
  results since a policy update in March 2021.
* Removed two illustrations from the vignette due to tarball size > 5 MB on 
  MacOs. Links to the fully illustrated vignette on the package's website are 
  now included in the vignette and in the README.


# viscomplexr 1.1.0

Published on CRAN 11. Dec 2020

Implemented the function *phasePortraitBw* which allows for phase portraits 
based on polar chessboard grids. Such plots have a special aesthetic appeal and 
can at the same time support the interpretation of standard phase portraits.

# viscomplexr 1.0.1

Published on CRAN 3. Nov 2020

Fixed the issues pointed out by Gregor Seyer in his review of the CRAN 
submission:

* Unwrapped all examples from \\dontrun{}. Most of them had to be wrapped in 
  \\donttest{}, however, due to execution time > 5 sec
* Implemented the option *verbose* in *phasePortrait* which allows to suppress 
  all progress messages
* The environments created in order to allow handling large arrays by pointers 
  do now have the empty environment as their parent (not the global environment)
* Made sure that no example, neither in the documentation of the functions, nor 
  in the vignette, does use more than two processor cores

# viscomplexr 1.0.0

* Added a `NEWS.md` file to track changes to the package.
* Submitted version 1.0.0 to CRAN
