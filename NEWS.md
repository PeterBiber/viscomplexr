# viscomplexr 1.0.1

Fixed the issues pointed out by Gregor Seyer in his review of the CRAN submission:

* Unwrapped all examples from \\dontrun{}. Most of them had to be wrapped in \\donttest{}, however, due to execution time > 5 sec
* Implemented the option *verbose* in *phasePortrait* which allows to suppress all progress messages
* The environments created in order to allow handling large arrays by pointers do now have the empty environment as their parent (not the global environment)
* Made sure that no example, neither in the documentation of the functions, nor in the vignette, does use more than two processor cores

------
# viscomplexr 1.0.0

* Added a `NEWS.md` file to track changes to the package.
* Submitted version 1.0.0 to CRAN
