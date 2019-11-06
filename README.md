
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmdt <img src="brain.gif" height="150px" width="150px" align="right" />

`mmdt` is an R package for conducting the multi-modal density test
(associated paper currently under revision). This package creates data
structures necessary for applying the method to imaging data, then
allows the user to perform the analyses, summarize the results, and
create figures for visualization.

## Installation

To get the latest development version from GitHub:

``` r
devtools::install_github('jdwor/mmdt')
```

[![Build
Status](https://travis-ci.com/jdwor/mmdt.svg?token=rLYL5VRxjrkzYHqLNKBX&branch=master)](https://travis-ci.com/jdwor/mmdt)

## Functions

Below is a list of the functions and a description of options available
to utilize through the `mmdt` package. An example of how to run the
analysis from beginning to end is given below, in the “Vignette”
section.

### get.mmdt.obj

This function creates an ‘mmdt object’ from vectors of nifti filenames,
subject IDs, and subject group memberships. This information is compiled
into a data structure that can be entered into the ‘mmdt’ function to
perform the analysis.

``` r
get.mmdt.obj(masks, modal1, modal2, modal3=NULL,
             modal4=NULL, modal5=NULL, modal6=NULL,
             ids, groups, parallel=TRUE, cores=2, pb=TRUE)
```

*Arguments*

  - `masks` A vector of class  that gives .nii or .nii.gz filenames for
    subjects’ masks. Masks demarcate which voxels should be included in
    the analysis, and are coded by TRUE/FALSE or 1/0.
  - `modal#` Vectors of class  that give .nii or .nii.gz filenames for a
    given imaging modality across subjects. At least two modalities
    (modal1 and modal2) must be entered. Up to 6 can be included.
  - `ids` A vector of subject ids. Must be the same length as the
    filenames in the ‘modal\#’ vectors.
  - `groups` A vector of group membership. Must be two categories, and
    should be the same length as ‘ids’.
  - `parallel` A logical value that indicates whether the user’s
    computer should run the code in parallel.
  - `cores` If parallel = TRUE, cores is an integer value that indicates
    how many cores the function should be run on.
  - `pb` A logical value that indicates whether or not a progress bar
    will be shown during analysis.

### mmdt

This function runs the multi-modal density test (mmdt) using an mmdt
object obtained from ‘get.mmdt.obj’.

``` r
mmdt(mmdt.obj, mins=NULL, maxs=NULL,
     gridsize=NULL, H=NULL, mc.adjust="BH",
     nperm=500, parallel=TRUE, cores=2, pb=TRUE)
```

*Arguments*

  - `mmdt.obj` An mmdt object obtained using the ‘get.mmdt.obj’
    function.
  - `mins` A vector giving the lower intensity bounds for each modality.
    If NULL, lower bounds will be set to the minimum observed value for
    each modality.
  - `maxs` A vector giving the upper intensity bounds for each modality.
    If NULL, upper bounds will be set to the maximum observed value for
    each modality.
  - `gridsize` A vector giving the number of points along each dimension
    at which the densities should be evaluated and tested. If NULL, this
    value defaults to 151x151 for two modalities, 51x51x51 for three,
    and 21x21x21x21 for four. Must be specified manually when analyzing
    4-6 modalities.
  - `H` The bandwidth matrix used for kernel density estimation. If
    NULL, a plug-in bandwidth estimator is used.
  - `mc.adjust` A character vector giving the multiple comparison
    adjustments to use. Default is “BH”, which controls FDR using the
    Benjamini-Hochberg procedure. The additional options are: “BY”,
    which controls FDR using the Benjamini-Yekutieli procedure, “maxt”,
    which controls FWER using max-t correction, and “tfce”, which
    controls FWER using threshold-free cluster enhancement. Both of the
    latter options use permutation to determine significance.
  - `nperm` If mc.adjust contains either ‘maxt’ or ’tfce, this is an
    integer value that gives the number of permutations desired to
    estimate the null distribution.
  - `parallel` A logical value that indicates whether the user’s
    computer should run the code in parallel.
  - `cores` If parallel = TRUE, cores is an integer value that indicates
    how many cores the function should be run on.
  - `pb` A logical value that indicates whether or not a progress bar
    will be shown during analysis.

### summarize.mmdt

This function outputs a summary of the mmdt output, printing whether or
not there were significant differences after adjustment for multiple
comparisons, and giving the approximate locations of differences within
the density space.

``` r
summarize.mmdt(mmdt.results)
```

*Arguments*

  - `mmdt.results` An object resulting from the ‘mmdt’ command.

### fig.mmdt

This function creates visualizations of the mmdt results (either a
t-statistic map or a significance map).

``` r
fig.mmdt(mmdt.results, type="significance", 
         mc.adjust="BH", coords=c(NA,NA))
```

*Arguments*

  - `mmdt.results` An object resulting from the ‘mmdt’ command.
  - `type` Type of image to be produced. Can be “t-statistic” or
    “significance”. Default is “significance”.
  - `mc.adjust` If type=“significance”, this states which adjustment
    method to use for visualization.
  - `coords` If more than two modalities were used to create
    ‘mmdt.results’ object, this gives a vector of length d \[e.g.,
    c(NA, NA, 3.25) for d=3\] giving the coordinates at which the plane
    should be visualized. Entries should be “NA” for the two modalities
    to be plotted along the x and y axes, and other entries should give
    the value along the each other dimensions at which the results
    should be visualized.

### mmdt.to.brain

This function maps mmdt results back onto subjects’ brain image domains
for visualization and exploration purposes.

``` r
mmdt.to.brain(mmdt.results, type="t-statistic", mask, 
              modal1, modal2, modal3=NULL, modal4=NULL,
              modal5=NULL, modal6=NULL)
```

*Arguments*

  - `mmdt.results` An object resulting from the ‘mmdt’ command.
  - `type` Type of image to be produced. Can be “t-statistic” or
    “significance”. Default is “significance”.
  - `mask` A string that gives a .nii or .nii.gz filename for the given
    subject’s mask. Masks will demarcate which voxels will be included,
    should be coded by TRUE/FALSE or 1/0, and should be the same as the
    masks used to conduct the mmdt analyses.
  - `modal#` Strings that give a .nii or .nii.gz filename for a
    subject’s given imaging modality. At least two modalities (modal1
    and modal2) must be entered. Up to 6 can be included. The same
    modalities used in the mmdt analyses should be entered here, in the
    same order.

## Vignette
