# projetProgProb

## Dependencies
```
opam install . --deps-only
```

## Running examples:

To run examples run `cd ` to the cloned directory and run:
```
dune exec ./examples/<name>.exe 
```


## Example usage
### draw a random triangle
few examples are provided, one of them is ` ./examples/triangle.ml` it contains the sampling model
you can edit this file to change the sampling method `(open...)` line and the lower and upper. 
Once edited run (keep the text file name):
```
dune exec ./examples/triangles.exe > triangle.txt
```

Then you can use any tool like gnuplot to visualize the outcome, a ready to use script visualize the sampled triangle is provided, just run:

```
python rand_triangle.py
```
Enjoy changing models and compare different patterns.
