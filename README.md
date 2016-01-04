[![Build Status](https://travis-ci.org/jhoffjann/nmmso.R.svg)](https://travis-ci.org/jhoffjann/nmmso.R)
# nmmso.R
Multimodal Optimization Method - R Seminar Project
An attempt to implement the [NMMSO Algorithm by Jonathan E. Fieldsend](https://github.com/fieldsend/ieee_cec_2014_nmmso) in R.

## Installation
Since this package is currently not on CRAN and a WIP Version you can install it with the wonderful [devtools](https://github.com/hadley/devtools) by Hadley. The command for this is
	
	 devtools::install_github("jhoffjann/nmmso.R")

Alternatively you can of course just clone this and load it yourself and start contributing.


## Test Suite
We also reimplemented the CEC Benchmarking Suite in R which can be used to test this algorithm. You can find it [here](https://github.com/jhoffjann/nmmso_benchmark)

## Technical Documentation
The Technical Documentation of this can be found in the benchmarking repository. Or just [here](https://github.com/jhoffjann/nmmso_benchmark/blob/master/documentation/doc.pdf)

## PR and SR Test Data
The Test Data which is formatted as requested by the CEC can also be found in the benchmarking repository ([SR](https://github.com/jhoffjann/nmmso_benchmark/blob/master/nmmsor_sr.dat), [PR](https://github.com/jhoffjann/nmmso_benchmark/blob/master/nmmsor_pr.dat))

## Important Links
The test suite - http://goanna.cs.rmit.edu.au/~xiaodong/cec15-niching/competition/    
The style guide - https://github.com/mikeagn/CEC2013/tree/master/NichingCompetition2013FinalData    
The test data - https://github.com/fieldsend/IEEE_CEC_2015_niching_competition/

