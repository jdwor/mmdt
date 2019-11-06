
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
to utilize through the `mimosa` package.

### get.mmdt.obj

This function calculates true positive rate, false positive rate, false
negative rate, false positive count, and sensitivity.

Formulas for how these are calculated are provided in the ‘Evaluate
Performance’ section.

``` r
count_stats(gold_standard, 
            predicted_segmentation, 
            k, 
            percent_overlap = NULL, 
            verbose = TRUE)
```

*Arguments*

  - `gold_standard` Gold standard segmentation mask of class `nifti`
  - `predicted_segmentation` Predicted segmentation mask volume of class
    `nifti`
  - `k` Minimum number of voxels for a segmentation cluster/component
  - `percent_overlap` Proportion of gold standard segmentation to be
    overlapped by predicted
  - `verbose` Logical indicating printing diagnostic output

### mmdt

This function calculates true positive rate, false positive rate, false
negative rate, false positive count, and sensitivity.

Formulas for how these are calculated are provided in the ‘Evaluate
Performance’ section.

``` r
count_stats(gold_standard, 
            predicted_segmentation, 
            k, 
            percent_overlap = NULL, 
            verbose = TRUE)
```

*Arguments*

  - `gold_standard` Gold standard segmentation mask of class `nifti`
  - `predicted_segmentation` Predicted segmentation mask volume of class
    `nifti`
  - `k` Minimum number of voxels for a segmentation cluster/component
  - `percent_overlap` Proportion of gold standard segmentation to be
    overlapped by predicted
  - `verbose` Logical indicating printing diagnostic output

### summarize.mmdt

This function calculates true positive rate, false positive rate, false
negative rate, false positive count, and sensitivity.

Formulas for how these are calculated are provided in the ‘Evaluate
Performance’ section.

``` r
count_stats(gold_standard, 
            predicted_segmentation, 
            k, 
            percent_overlap = NULL, 
            verbose = TRUE)
```

*Arguments*

  - `gold_standard` Gold standard segmentation mask of class `nifti`
  - `predicted_segmentation` Predicted segmentation mask volume of class
    `nifti`
  - `k` Minimum number of voxels for a segmentation cluster/component
  - `percent_overlap` Proportion of gold standard segmentation to be
    overlapped by predicted
  - `verbose` Logical indicating printing diagnostic output

### fig.mmdt

This function calculates true positive rate, false positive rate, false
negative rate, false positive count, and sensitivity.

Formulas for how these are calculated are provided in the ‘Evaluate
Performance’ section.

``` r
count_stats(gold_standard, 
            predicted_segmentation, 
            k, 
            percent_overlap = NULL, 
            verbose = TRUE)
```

*Arguments*

  - `gold_standard` Gold standard segmentation mask of class `nifti`
  - `predicted_segmentation` Predicted segmentation mask volume of class
    `nifti`
  - `k` Minimum number of voxels for a segmentation cluster/component
  - `percent_overlap` Proportion of gold standard segmentation to be
    overlapped by predicted
  - `verbose` Logical indicating printing diagnostic output

### mmdt.to.brain

This function calculates true positive rate, false positive rate, false
negative rate, false positive count, and sensitivity.

Formulas for how these are calculated are provided in the ‘Evaluate
Performance’ section.

``` r
count_stats(gold_standard, 
            predicted_segmentation, 
            k, 
            percent_overlap = NULL, 
            verbose = TRUE)
```

*Arguments*

  - `gold_standard` Gold standard segmentation mask of class `nifti`
  - `predicted_segmentation` Predicted segmentation mask volume of class
    `nifti`
  - `k` Minimum number of voxels for a segmentation cluster/component
  - `percent_overlap` Proportion of gold standard segmentation to be
    overlapped by predicted
  - `verbose` Logical indicating printing diagnostic output
