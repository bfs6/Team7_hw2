[![wercker status](https://app.wercker.com/status/0d48a75b6b9a31b18516a99fc2561501/m "wercker status")](https://app.wercker.com/project/bykey/0d48a75b6b9a31b18516a99fc2561501)

# Team7_hw2

1. is_valid(g)
The is_valid function checks all possible cases of invalid graphs. If the graph
passes all the test cases for invalid graphs, the function returns TRUE.
First check if the graph is a list of lists
then check if there are missing labels in the graph
then check if the graph has invalid structure. For example, missing edges or missing weights
then check if there are duplicate labels in the graph
then check if there is invalid vertex reference. For example, when the edges component in the secondary list refers to a nonexistence vertex
then check if there are duplicate vertices
then check if all the edges are stored as integers
then check if all the weights are stored as doubles
then check if there are duplicate edges, for example if a vertex is connected to the same vertex twice
then check if there is a weight assigned for every edge
if all these cases pass, then the graph is valid


2. is_undirected(g)
First check if the graph is valid using the is_valid function. If the graph is not valid then throw an error. 
If the graph is valid, create an adjacency matrix of the graph using helper function adj_matrix. Then check if the transpose 
of the adjacency matrix is equal to the adjacency matrix, thus meaning that there is a edge from a node i to j and vice versa. 
We did this by running a double forloop and checking if the value of the index at (i, j) is equal to that at (j, i)

3. is_isomorphic(g1, g2)
First check if both graphs are valid. If either isn't then throw an error. Then I checked if the graphs have the same number of vertices with the same names. 
If not, the function would return false. Lastly I would create two adjacency matrices for both graphs find their respective edges from one vertex to another. 
If those edges did not have the same weight then I would return false. If every edge had the same weight that means the two graphs are isomorphic. 


4. is_connected(g, v1, v2)
First I checked if the graph is valid. If not the function threw an error. Then I checked if the verticies are characters. 
If they were any other data structure the function would throw an error. Then I ran a BFS search starting at vertex v1 to see if we can 
reach vertex v2, keeping the verticies that we can traverse in a integer list containing their index. However instead of initially placing
the index of v1 to the integer list called visited as implemented in most BFS algorithms, I did not include v1. 
This is because v1 can equal v2 and that we have a directed graph. 
This to avoid this conflict, this modified BFS would search the graph and see if we can reach v1 from v1 as well.

5. shortest_path(g, v1, v2)
I first ran is_connected(g, v1, v2). If is_connected threw an error, then I would stop the function. If it returned false, then the function would return an empty character array. 
If the two verticies were connected, then I had to find the shortest path. The way I did this was to run dijkstras algorithm to find the shortest distance.
My implementation of dijkstras finds the shortest distance from vertex v1 to all other verticies stored in an integer array. It also computes an array of indices that is the previous vertex to the destination vertex 
if we were to start at v1. Using this given information I can trace the path from any vertex v2 back to v1 granted that v2 is not v1, thus giving me the path. 
However for the case where v1 equals v2, we have to find the shortest cycle that contains v1. To find the shortest cycle in this scenario, I ran djikstras algorithm to 
compute an array of the distance of the shortest path to each other vertex and set the distance from v1 to itself as 0. 
Then I iterated through all edges of each vertex to see if any of those edges were directed back to v1 thus detecting a cycle. 
If so, then we know that a cycle which has distance equal to the sum of the edge's weight directed back to v1 and dijkstra's shortest distance from v1 to that vertex. 
I would then find the shortest cycle amongst all cycles and output the corresponding path. 




