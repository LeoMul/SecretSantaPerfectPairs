# SecretSantaPerfectPairs
What are the chances of two people having one another in Secret Santa? 
This quantity, a perfect pair, appears to be incredibly likely, and is 
calculated here. 
```
gfortran main.f90 -o main.x -{your flags}
```
The executable is obviously named to your liking. 
The code expects an input file called ss.inp. If this is not in the running directory,
an example is printed with,
```
./main.x
```
To use it you can use 
```
./main.x > ss.inp
```
where warning message won't be piped in as it is forced to stderr. 