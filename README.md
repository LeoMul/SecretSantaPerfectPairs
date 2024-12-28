# SecretSantaPerfectPairs
What are the chances of two people having one another in Secret Santa? 
This quantity, a perfect pair, appears to be incredibly likely, and is 
calculated here. 
```
gfortran main.f90 -o main.x -{your flags}
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
'''
&SecretSantaInput 
num_samples = 10000
num_pops = 3
/
2
500
2000
''' 
will perform 10_000 samples on the system sizes 2, 500 and 2000.