#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
using namespace std;

#define max_size 1000001

int arrived[max_size], finished[max_size], parent[max_size];
vector<int> *adj;

void print_v(vector<int> v [], unsigned size) {
	for (unsigned i = 0; i < size; i++) {
		cout << i;
		for (unsigned j = 0; j < v[i].size(); j++) cout << " " << v[i][j];
		cout << endl;
	}
}

//Initialisation of arrays
void initialise_arrays(int N) {
	for (int j = 0; j < N; j++) {
		arrived[j] = 0;
		finished[j] = 0;
		parent[j] = -1;
	}
}

//Fill global arrays arrived, left, parent and find two vertices that belong to cycle if one exists
void DFS(int n, int &t, int &end, int &start) {
	arrived[n] = ++t;
	for (unsigned i = 0; i < adj[n].size(); i++) {
		if (arrived[adj[n][i]] == 0) {
			parent[adj[n][i]] = n;
			DFS(adj[n][i], t, end, start);
		}

		else if (arrived[n] > arrived[adj[n][i]] && finished[adj[n][i]] == 0 && adj[n][i] != parent[n]) {
			end = n; 
			start = adj[n][i]; 
		}
	}

	finished[n] = ++t;
}

//Returns number of nodes of tree
int DFS_count(int n) {
	int count = 1;
	arrived[n] = 1;
	for (unsigned i = 0; i < adj[n].size(); i++) {
		if (arrived[adj[n][i]] == 0)
			count += DFS_count(adj[n][i]);
	}
	return count;
}

//Removes both (u,v) and (v,u)
//Returns true only if the edge existed
bool remove_edge(int u, int v) {
	vector<int>::iterator pos_1 = find(adj[u].begin(), adj[u].end(), v);
	vector<int>::iterator pos_2 = find(adj[v].begin(), adj[v].end(), u);
	if (pos_1 != adj[u].end() && pos_2 != adj[v].end()) {
		adj[u].erase(pos_1);
		adj[v].erase(pos_2);
		return true;
	}
	
	else return false;
}

//If a cycle exists it returns true and fills path vector with nodes
bool find_cycle(vector<int> &path, int V) {
	int end_of_cycle = -1, start_of_cycle = -1;
	int time = 0;
	for (int node = 1; node < V; node++) {
		if (arrived[node] == 0) DFS(node, time, end_of_cycle, start_of_cycle);
	}

	if (end_of_cycle == -1) return false;

	else {
		int index = end_of_cycle;
		while (index != parent[start_of_cycle]) {
			path.push_back(index);
			index = parent[index];
		}

		reverse(path.begin(), path.end());
	}

	return true;
}

int main(int argc, char **argv)
{
	ifstream infile;
	infile.open(argv[1]);
	int T, N, M, u, v;
	vector<int> cycle, cycle_extra;

	infile >> T; 
	for (int i = 0; i < T; i++) {
		infile >> N >> M; 
		N++; //Arrays, vectors are zero indexed but input starts with one
		delete[] adj;
		adj = new vector<int>[N];

		//Read edges and fill adjacency
		//Each edge is added twice to create undirected graph
		for (int j = 0; j < M; j++) {
			infile >> u >> v; //change with infile
			adj[u].push_back(v);
			adj[v].push_back(u);
		}

		//Initialisation
		initialise_arrays(N);

		bool flag = find_cycle(cycle, N);
		if (!flag) cout << "NO CORONA" << endl;
		else {
			//Removing one edge of the cycle doesn't affect connectivity of graph
			//If another cycle is found after removing it then we have two cycles
			remove_edge(cycle[0], cycle[cycle.size() - 1]);
			
			//Initialisation
			initialise_arrays(N);
			
			flag = find_cycle(cycle_extra, N);

			if (flag) cout << "NO CORONA" << endl;

			//There is at least one cycle in the graph
			else {
				//Remove all edges of cycle
				for (unsigned j = 1; j < cycle.size(); j++) remove_edge(cycle[j - 1], cycle[j]);

				//Run DFS for each of the vertices of cycle that returns size of trees
				vector<int> tree_sizes;
				
				//Initialisation
				initialise_arrays(N);
				
				for (unsigned j = 0; j < cycle.size(); j++) tree_sizes.push_back(DFS_count(cycle[j]));

				//sort array of sizes
				sort(tree_sizes.begin(), tree_sizes.end());

				cout << "CORONA " << cycle.size() << endl;
				for (unsigned j = 0; j < tree_sizes.size(); j++) cout << tree_sizes[j] << " ";
				cout << endl;
			}
		}
	}
}
