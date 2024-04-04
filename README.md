<img src="https://github.com/colombarifm/themis/blob/master/biphenyl_test/OPD_biphenyl.png" width="75%" height="75%">

<br />

This is a software written for the calculation of the scaled Osipov-Pickup-Dunmur
chirality index, as reported on:

It reads a simple .xyz coordinate file and returns both scaled (G0,s) and unscaled
(G0) values.

This is a free software written in Fortran 2003 language, being available at
https://github.com/colombarifm/OPD_chirality_index under the GPLv3+ License. 
It was developed and tested under Linux environment with gfortran 7.5+ compilers.  

# Install

See [installation instructions](./INSTALL.md)  

# Links for useful articles

* [CENTER_OF_MASS](https://github.com/colombarifm/center_of_mass) program to calculate the center of mass of molecular structures
* [SAS_GRID](https://github.com/colombarifm/sas_grid) program to generate the solvent accessible surface translation grid for molecular structures

# Directory organization

* [`src`](./src): The source code
* [`biphenyl_test`](./biphenyl_test): Input files for the calculation of the torsional profile of biphenyl

