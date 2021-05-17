# Changens in v0.7.3

* Update tests for quanteda v3.0
* Change predicted values to factor with all labels in training data

# Changes in v0.7.2

* Make `predict()` significantly faster

# Changes in v0.7.1

* Improve efficiency of `textmodel_newsmap()`
* Add compatibility with newer `textstat_entropy()`

# Changes in v0.7.0

* Add Hebrew and Arabic seed dictionaries

# Changes in v0.6.9

* Clean up Italian, German and Spanish seed dictionaries

# Changes in v0.6.8

* Add Italian seed dictionary

# Changes in v0.6.7

## Dictionary updates

* Correct Japanese seed words for DE, MG and EC 

# Changes in v0.6.4

## Bug fixes

* Return NA for documents that do not have known features 
* Drop document variables to avoid slowdown and warnings

## New data

* Add Chinese (simplified and traditional) seed dictionaries
* Add French seed dictionary

## New function

* Add a function to compute average feature entropy (AFE)

# Changes in v0.6.0  

## Bug fixes

* Fix error in textmodel_newsmap() when smooth is < 1.0
* Fitted models no longer include classes that did not occur in training set

# Changes in v0.4.6

## New functions

* Add coef() and coefficients() methods

## Dictionary updates

* Add Russian language seed dictionary
* Correct Cook Islands' country code from CC to CK, and remove it from POL
* De facto capital cities of TZ, ZA, NG and BO are added to all dictionaries
* CC is added to DE, JA and EN dictionaries
* Dictionary entries are sorted in alphabetical order of country code
