## Update submission (version 1.1.2)
* Corrected language entry in DESCRIPTION to "en-US" instead of "en_US" which
  caused a CRAN note.
* Fixed a "lost-braces" note from R-CMD-check caused by a typo in the 
  documentation.

### Test environments
* local ubuntu-24.10, R-4.4.2




## Update submission (version 1.1.1)
Three improvements were made for version 1.1.1:

* The default setting of how many processor cores to be used by the functions
  *phasePortrait* and *phasePortraitBw* was reduced to one core less than
  available.
* The LazyData specification was removed from the DESCRIPTION as the package
  does not comprise any data. This avoids a NOTE in the CRAN package check
  results since a policy update in March 2021.
* Two illustrations were removed from the vignette because new tests on MacOs
  indicated a tarball size slightly beyond the 5 MB allowed on CRAN. Links to
  the fully illustrated version of the vignette on the package's website were
  included in the vignette and in the README.


### Test environments
* ubuntu-20.04 on github, R-devel
* ubuntu-20.04 on github, R-4.1.1
* windows-latest on github, R-4.1.1
* windows server 2008 (64-bit) on win-builder, R-devel
* windows server 2008 (64-bit) on win-builder, R-4.1.1
* windows server 2008 (64-bit) on win-builder, R-4.0.5
* macOS-latest on github, R 4.1.1
* local manjaro (Kernel 5.14.2-1-MANJARO (64-bit)) install, R.4.1.1

### R CMD check results
0 errors | 0 warnings | 0 notes,

except for the local Manjaro install, where R CMD check returns the 
note _Compilation used the following non-portable flag(s): ‘-march=x86-64’_.
This is, however, a local setting that will not affect the install on other
machines.

### Downstream dependencies
There are currently no downstream dependencies for this package.




## Update submission (version 1.1.0)
For version 1.1.0 the function *phasePortraitBw* was implemented which plots 
phase portraits based on polar chessboard grids. This is a substantial update; 
it virtually doubles the graphical scope provided by the package.

### Test environments
* local Ubuntu 20.04 install, R 4.0.3
* local Windows 8.1 install, R 4.0.3
* local Windows 10 install, R 4.0.2
* windows-latest (Microsoft Windows Server 2019) on github, R 4.0.3
* macOS-latest (Mac OS X) on github, R 4.0.3
* ubuntu-20.04 (release) on github, R 4.0.3
* ubuntu-20.04 (devel) on github, R-devel

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
There are currently no downstream dependencies for this package.


## Resubmission
Thanks to Gregor Seyer for his constructive review of the first submission. I 
fixed all issues pointed out. Find below G. Seyer's remarks and my answers to 
each of them.

**G.S.:** *\\dontrun{} should only be used if the example really cannot be 
executed (e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \\dontrun{} adds the comment 
("# Not run:") as a warning for the user. Does not seem necessary. Please unwrap
the examples if they are executable in < 5 sec, or replace \\dontrun{} with
\\donttest{}.*

**Answer:** I unwrapped all examples. Most of them, however, take > 5 sec 
execution time. Therefore, I encapsulated these with \\donttest{}.

**G.S.:** *You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to extract
the information a user is interested in, and then print() that object. Instead 
of print()/cat() rather use message()/warning() or if(verbose) cat(..) (or maybe
stop()) if you really have to write text to the console. (except for print, 
summary, interactive functions)*

**Answer:** The messages can now be easily suppressed by calling *phasePortrait*
with *verbose = FALSE*. The default value of *verbose* is TRUE, because 
calculation may take up to several minutes (and more, in extreme cases), and the
purpose of the messages is to inform the user in real time about the progress. 
After completion, the information is not really useful anymore, that's why it is
not stored and returned in an object. Needless to say, the use of *verbose* is
included in the documentation and also mentioned in the first example shown in
the vignette.

**G.S.:** *Please do not modify the global environment in your functions. This
is not allowed by the CRAN policies.*

**Answer:** Fixed. For being able to handle large arrays with pointers to them,
I create environments which contain these arrays. In the previous version, the 
parent of these environments was the global environment. Now, the parent is the 
empty environment.

**G.S.:** *Please ensure that you do not use more than 2 cores in your examples,
vignettes, etc.*

**Answer:** Done.

### Test environments
* local Ubuntu 20.04 install, R 4.0.3
* local Windows 8.1 install, R 4.0.3
* local Windows 10 install, R 4.0.2
* windows-latest (Microsoft Windows Server 2019) on github, R 4.0.3
* macOS-latest (Mac OS X) on github, R 4.0.3
* ubuntu-20.04 (release) on github, R 4.0.3
* ubuntu-20.04 (devel) on github, R-devel

### R CMD check results
0 errors | 0 warnings | 0 notes



-------------
## First submission
* This is the first submission of the package viscomplexr

### Test environments
* local Ubuntu 20.04 install, R 4.0.2
* local Windows 8.1 install, R 4.0.2
* windows-latest (Microsoft Windows Server 2019) on github, R 4.0.2
* macOS-latest (Mac OS X) on github, R 4.0.2
* ubuntu-20.04 (release) on github, R 4.0.2
* ubuntu-20.04 (devel) on github, R-devel

### R CMD check results
0 errors | 0 warnings | 0 notes


