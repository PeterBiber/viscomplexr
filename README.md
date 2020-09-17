
<!-- README.md is generated from README.Rmd. Please edit that file -->

# viscomplexr

<!-- badges: start -->

<!-- badges: end -->

<!-- Make a pretty banner. The code is executed but not displayed here. Below, it is displayed in the example section, but not exectued -->

<img src="man/figures/README-banner-1.png" width="100%" style="display: block; margin: auto;" />

<br />

With *viscomplexr* you can create phase portraits of functions on the
complex number plane. While the main purpose of this package are
scientific, educational and technical applications, the sheer beauty of
phase portraits may be motivation enough to explore the fascinating
realm of complex-valued functions. The package is embedded in the
framework of R base graphics and allows for print quality graphics
files. To a great deal, *viscomplexr* follows the conventions published
by Wegert (2012).

For a detailed step-by-step introduction see the package’s vignette.
After installing *viscomplexr* you can view the vignette by calling:

``` r
vignette("viscomplexr-vignette")
```

## Installation

Hopefully soon you will be able to install the released version of
*viscomplexr* from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("viscomplexr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PeterBiber/viscomplexr")
```

## Example

Let’s plot a phase portrait of the function `f(z)=sin(z^3/(z-1i+2))`.
This function is portrayed in the banner above, and the code below
produces exactly that banner. The workhorse function of *viscomplexr* is
called `phasePortrait`, and besides its specific input variables, it
also accepts all parameters that are accepted by R’s `plot.default`
function, like `axes`, `xaxs`, and `yaxs` in the example below. Other
often useful ones are `main`, `xlab`, and `ylab`, especially for
scientific, technical or educational applications.

``` r
# Make a phase portrait of the function 
# f(z) = sin(z^3/(z - 1i + 2))
# in the domain [-6, 6] (real), and [-2.4, 2.4] (imaginary)

library(viscomplexr)

# You can open any R graphics device here (bmp, jpeg, png, tiff, pdf,
# windows, x11, ...), see R documentation if you are not familiar with these.
# This example, as it is, plots into the default graphics device.

# reduce plot margin to 0, store previous graphics parameter settings in op
op <- par(mar = c(0, 0, 0, 0))
# call the function phasePortrait
phasePortrait("sin(z^3/(z - 1i + 2))",               # define the function
              xlim = c(-6, 6), ylim = c(-2.4, 2.4),  # define the domain
              axes = FALSE,                          # suppress axes
              xaxs = "i", yaxs = "i")          # no empty zone around plot
# set graphics parameters to previous values
par(op)

# If you have initially opened a graphics device that plots into a file, you 
# must close it here with
# dev.off()
```

While there are many options and a few useful supporting functions (see
vignette and documentation), this example shows the core way of plotting
phase portraits. It could even be reduced to just the function to be
portrayed (here provided as a character string) and the parameters
`xlim` and `ylim`. The other parameters in the example, and also the
omission of the plot margins before calling *phasePortrait*, were only
used in order to obtain a pretty banner for this README.

## References

<div id="refs" class="references">

<div id="ref-wegert_visualcpx_2012">

Wegert, Elias. 2012. *Visual Complex Functions. An Introduction with
Phase Portraits*. Basel Heidelberg New York Dordrecht London: Springer.

</div>

</div>
