# SecretSantaPerfectPairs
What are the chances of two people having one another in Secret Santa? 
This quantity, a perfect pair, appears to be incredibly likely, and is 
calculated here. Compile the code with
```
gfortran main.f90 -o SecretSantaPP.x -{your flags}
```
The executable is obviously named to your liking. 
The code expects an input file called _ss.inp_. If this is not in the running directory,
an example is printed with,
```
./main.x
```
To use it you can use 
```
./main.x > ss.inp
```
where warning message won't be piped in as it is forced to stderr. The input is a fortran namelist,
specifying the number of random samples taken per system size, and the number of system sizes to be checked. The specific system sizes to be checked are then put under the input, for example: 
```
&SecretSantaInput 
num_samples = 10000
num_pops = 3
/
5
6
7
```
will perform 10_000 samples on the system sizes 5, 6 and 7. Running
```
./main.x
```
with an input in your directory will produce an output like
```
#NumPerfPairs; N =           5          6          7
            0          0.54810    0.61180    0.61720
            1          0.45190    0.33100    0.27040
            2          0.00000    0.00000    0.11240
            3          0.00000    0.05720    0.00000
            4          0.00000    0.00000    0.00000
            5          0.00000    0.00000    0.00000
            6          0.00000    0.00000    0.00000
```
You can pipe this into a formatted file of your choosing with 
```
./main.x > {your file name}
```
Possible upgrades to the code include OpenMP parallelisation for the sampling, or perhaps even a large-deviation implementation to sample huge system sizes, where in principle all parties can be in a perfect pair (for even system size). This would allow an indirect sampling of distribution, see for example _Wang F and Landau D P 2001 Phys. Rev. Lett. 86 2050_. This would probably be done in Rust, where Yannick Feld has a library for this, see for example https://github.com/Pardoxa/sampling, which has been used in studies like _Feld Y et al 2022 Physical Review E 105 (3), 034313_ and  _Leo Patrick Mulholland et al 2023 New J. Phys. 25 113034_.