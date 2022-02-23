# projetProgProb

## Running examples:

To run examples run `cd ` to the cloned directory and run:
```
dune exec ./examples/<name>.exe  [-verbose]
```


## Example usage
### draw a random triangle
few examples are provided, one of them is ` ./examples/triangle.ml` it contains the sampling model
you can edit this file to change the sampling method `(open...)` line and the lower and upper. 
Once edited run (keep the text file name):
```
dune exec ./examples/triangles.exe >> triangle.txt
```

To visualize the drawn triangle run:

```
python rand_triangle.py
```
Enjoy changing models and compare different patterns.
