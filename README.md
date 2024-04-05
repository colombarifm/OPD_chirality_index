# *opd* program for the calculation of the scaled Osipov-Pickup-Dunmur chirality index

## Install

See [installation instructions](./INSTALL.md)  


## Usage

It reads a simple .xyz coordinate file and returns both scaled (G0,s) and unscaled (G0) values

```bash
./opd --input <file.xyz>  > output.log
```

It can also run in parallel by setting the threads for OpenMP

```bash
export OMP_NUM_THREADS=<ncores>
```

As an example, the scaled Osipov chirality index (G0,s, as reported on Solymosi et al. 2002)
calculation was performed for multiple biphenyl conformations along its torsional profile 
(structures obtained at the GFN2-xTB level), as shown below:


<img src="https://github.com/colombarifm/opd_chirality_index/blob/main/biphenyl_test/OPD_biphenyl.png" width="75%" height="75%">

<br />

This is a free software written in Fortran 2003 language, being available at 
https://github.com/colombarifm/opd_chirality_index under the GPLv3+ License. 
It was developed and tested under Linux environment with gfortran 7.5+ compilers.  

## Links for important articles

* [Osipov (1995)](https://doi.org/10.1080/00268979500100831) M. A. Osipov, B. T. Pickup, D. A. Dunmur. *A new twist to molecular chirality: intrinsic chirality indices*. Mol. Phys., 84, 1193-1206 **(1995)**

* [Solymosi (2002)](https://doi.org/10.1063/1.1476321) M. Solymosi, R. J. Low, M. Grayson, M. P. Neal. *A generalized scaling of a chiral index for molecules*. J. Chem. Phys., 116, 9875-9881 **(2002)**

* [Hattne (2011)](https://doi.org/10.1098/rsif.2010.0297) J. Hattne, V. S. Lamzin. *A moment invariant for evaluating the chirality of three-dimensional objects*. L. R. Soc. Interface, 8, 144–151 **(2011)**


## Directory organization

* [`src`](./src): The source code
* [`biphenyl_test`](./biphenyl_test): Input files for the calculation of the torsional profile of biphenyl

