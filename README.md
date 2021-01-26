# hask-chess

## Notes

Changing order of generated moves starting from king, queen and most valueble pieces instead of the other way around has the following huge speedup 

```
benchmarking initial position/depth: 5 White
time                 3.446 s    (3.296 s .. 3.554 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.436 s    (3.397 s .. 3.467 s)
std dev              40.69 ms   (17.53 ms .. 54.18 ms)
variance introduced by outliers: 19% (moderately inflated)
```
to 

```
benchmarking initial position/depth: 5 White
time                 470.2 ms   (464.1 ms .. 475.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 469.3 ms   (466.8 ms .. 470.5 ms)
std dev              2.301 ms   (89.80 μs .. 2.860 ms)
variance introduced by outliers: 19% (moderately inflated)
```