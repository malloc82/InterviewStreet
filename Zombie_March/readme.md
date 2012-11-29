Zombie March
 
 
Zombies have placed themselves at every junction in New York. Each junction 'i' initially has a presence of ai number of zombies. At every timestep each zombie randomly chooses one of its neighboring junctions and walks towards it. Each neighboring junction is choosen by the zombie with an equal probability. In order to safegaurd the citizens of New York we need to find out the expected number of zombies at every junction after 'k' timesteps.
 
The network of New York is given as an edge list. 
 
Input Format:
 
t-> 'T' test cases, t test cases follow.
 
n, m, k- > number of junctions(nodes) of New York, number of roads (edges) and 'k' time steps.
 
Followed by m lines containing m edges, 1 edge in each line. Each edge is denoted by 2 integers that can range from 0 to n-1.  All the edges are bidirectional. A node cannot connect itself.
 
Followed by n  lines having initial number of Zombies at the location ai.
 
 
Constraints:
 
1<=t<=5
5<=n<=100000
1<=m<= 200000
1<=k<=10000000
1<=ai<=1000
 
Output Format:
 
No of zombies (rounded of to its nearest integer) in the Top 5 highly populated junctions after 'k' timesteps.
 
Sample Input:
 
1
10 18 100
0 8 
0 5 
1 2 
1 5 
2 8 
2 4 
2 5 
2 6 
3 5 
4 8 
4 6 
4 7 
5 8 
5 9 
6 8 
6 9 
7 9 
8 9 
1
1
1
1
1
1
1
1
1
1
 
Sample Output:
 
2 2 1 1 1
