# racmoR

**PRIVATE BRANCH OF THE pkg06_racmoR PACKAGE**

## Overview
racmoR is a personal package for interacting with and manipulating output from the RACMO regional climate model.
The package is an ongoing work in progress, is not foolproof, and is essentially a lot of syntactic wrapping around the [terra](https://rspatial.org) package to ease repeated processes.

It is necessary to separately download the RACMO data. 
The RACMO2.3p3 data that this package was initially created to handle can be accessed online at [https://doi.org/10.5281/zenodo.7639053] and was created by Christiaan van Dalum at IMAU, Utrecht University.

## Public Version
The public version of this repo can be accessed at: 
  https://github.com/polarSaunderson/racmoR

## To-Do
### 2023-08-14
- [ ] Check units in use_racmo_name

### 2023-08-11
- [ ] get_extent and related need to reproject an extent, not only SpatVectors or SpatRasters

### 2023-08-10
- [ ] Test racmoD_climatology
- [ ] Verify australSplit and removeIncomplete arguments
- [ ] should some of these be in terrapin?

### 2023-08-07
- [ ] Sort out terrapin's remove_incomplete_x functions

### 2023-08-06
- [ ] Check wording re:years in documentation of calc_x_racmo

### 2023-08-05
- [X] create_multiMonth_data_each_X; use functionals there too!
- [X] Could calculate_racmo_in_space be a closure? No, just use a functional.
 - [X] Need to do sum_in_space at least, but sd / median / percentiles / summary

### 2023-08-04
- [X] improve the crop_racmo functions
 - [X] simplify - not too many options!
 - [X] separate mask_racmo functions
- [X] The logic of get_shelf_outline and get_basin_outline need correcting
 - [X] They look only in their own dataset; need to look through all extents, then reduce to shelves
 - [X] Will impact draw_antarctica(), and crop_racmo() functions;
 - [X] check get_extent is not doing something wrong with this
- [X] Double check returnImbie in get_basin_outline; think about name and defaults too
- [X] Are separate exactExtents and rectangularExtents arguments necessary?
 - [X] get_extent, but also draw_antarctica and get_shelf_outline etc.

### 2023-08-03 
- [ ] get_extent uses %notIn%
- [ ] think about projection order in get_coastlines()
- [ ] update examples for get_extent based on imbieBasins argument
- [X] create crop_racmo functions
- [X] draw_antarctica function
- [ ] draw_racmo_axes functions
- [/] calculate_racmoX functions
- [ ] create_racmoX_NetCDFs

