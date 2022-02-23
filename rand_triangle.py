
import numpy as np
import matplotlib.pyplot as plt

#line = "10 30 40 45 80 90"


file1 = open('triangle.txt', 'r')
Lines = file1.readlines()
for line in Lines:

    points_arr = line.split()
    points = [int(element) for element in points_arr ]
    if len(points) < 6:  
        print ( "cant't do a triangle with less than 6 coordinates for 3 points")

    # Step 1
    x = points[0:2]
    y = points[2:4]
    z = points[4:6]

    X = np.array([ x, y, z]  )
    Y = ['red', 'green', 'blue']

    plt.figure()
    plt.scatter(X[:, 0], X[:, 1],s = 70, color = Y[:])

    t1 = plt.Polygon(X[:3,:], color = "gainsboro")
    plt.gca().add_patch(t1)
    plt.show()

