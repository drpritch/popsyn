ILUTE Population Synthesis Procedure
Version 1.0
David Pritchard

Required software:
* R (version 2.6.1 used for development), with a few extra packages
* C++ compiler to build two R modules
* Optional: PostgreSQL + PostGIS (8.1.10)
* Originally developed on a Debian Linux system, but there's no reason why
  it shouldn't work on other platforms.

SETUP
=====

1) Install R and required libraries:
  - RODBC
  - gtools
  - gdata
  - abind
  via the command install.packages('INSERT_LIBRARY_NAME_HERE');

2) Build C++-based R modules. Via the command line in Unix or Mac:
  MAKEFLAGS="CFLAGS=-O3" R CMD SHLIB ipf_list.cpp
  MAKEFLAGS="CFLAGS=-O3" R CMD SHLIB margintable2.c




RUNNING
=======
1) Input data comes from PostgreSQL database (default), or CSV files in
   ../data if desired. Change inputSource in inputData method in utils.R to
   select data source.
2) synthesize.R uses sparse IPF procedure to fit tables for various agent
   types. Outputs are stored to ../results/latest. Only one CMA is done at
   a time; select with cmaname variable (Toronto / HSK / Oshawa).
   Output files are fit* and prefit*
3) synthesize2.R uses nondeterministic conditional Monte Carlo procedure to
   build actual populations from fitted tables. Again, only one CMA is done
   at a time. Use cmaname and popseed variables to select CMA and random
   number seed for synthesis.
   Output files are pop*
4) pop.sql is used to merge the outputs from different CMAs; it's very
   simple, though, and not strictly necessary. All it does is make sure the
   agent IDs for the different CMAs don't collide.
5) experiments_* files are used to run experiments in thesis; not
   necessary for synthesis itself. experiments_mc_prep.sh is used to
   synthesize 30 populations with a fixed set of random number seeds

The procedure used here is essentially that described in David Pritchard's
thesis, with the following caveats:
* TTS zones are synthesized in addition to ctcode. This is essentially a
  postprocess, and is not guaranteed to give the right population totals
  for each TTS zone. I have tried to ensure that non-residential zones have
  zero population, however.
* St. Catharines, Niagara and Kitchener populations are fitted but not
  synthesized, leaving only Hamilton in the final HSK population.
