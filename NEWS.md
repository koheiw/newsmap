# Latest changes

* Correct Japanese seed words for DE, MG and EC 

# Changes in v0.6.5

## Bug fixes

* Return NA for documents that do not have known features 
* Drop document variables to avoid slowdown and warnings

## New data

* Add Chinese (simplified and traditional) seed dictionaries
* Add French seed dictionary

## New function

* Add a function to compute average feature entropy (AFE)

# Changes since newsmap v0.5

## Bug fixes

* Fix error in textmodel_newsmap() when smooth is < 1.0
* Fitted models no longer include classes that did not occure in training set

# Changes since newsmap v0.4.5

## New functions

* Add coef() and coefficients() methods

## Changes in dictionary

* Add Russian language seed dictionary
* Correct Cook Islands' country code from CC to CK, and remove it from POL

# Changes since newsmap v0.4.3

## Changes in dictionary

* De facto capital cities of TZ, ZA, NG and BO are added to all dictionaries
* CC is added to DE, JA and EN dictionaries
* Dictionary entries are sorted in alphabetical order of country code
