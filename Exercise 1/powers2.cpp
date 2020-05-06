#include <iostream>
#include <fstream>
using namespace std;
#define max_length 30

int decimal_toBinary(int n, int arr[max_length]) {
	int i = 0, count_digits = 0;
	while (n > 0) {
		count_digits += (arr[i] = n % 2);
		n /= 2;
		i++;
	}
	return count_digits;
}

//Prints array in format [x,y,.....]
void print_list(int arr[max_length]) {
	int pos_of_last_digit = 0;
	for (int i = 0; i < max_length; i++) {
		if (arr[i] > 0) pos_of_last_digit = i;
	}
	
	cout << "[" << arr[0];

	for (int i = 1; i <= pos_of_last_digit; i++) {
		cout << "," << arr[i];
	}

	cout << "]\n";
}

//Every time we loop through arr (start to end) we break
//one of the first power that we find (except for 2^0) to two
//of the previous power
//The result is the compact form of lexicographically least sum
void fix_powers(int k, int arr[max_length], int digits) {
	for (int i = digits; i < k; i++) {
		for (int j = 1; j < max_length; j++) {
			if (arr[j] > 0) {
				arr[j]--;
				arr[j - 1] += 2;
				break;
			}
		}
	}
	
}

void powers2(int N, int K, int arr[30]) {
	for (int i = 0; i < max_length; i++) arr[i] = 0;

	int digits = decimal_toBinary(N, arr);
	if (K < digits || K > N) cout << "[]\n";

	else {
		fix_powers(K, arr, digits);
		print_list(arr);
	}
}

int main(int argc, char **argv)
{
	//2^30 > 10^9 > 2^29
	//N requires at most 30 digits in binary
	int binary[max_length];
	
	ifstream file;
	file.open(argv[1]);
	int N, K, T;

	file >> T;
	for(int t = 0; t < T; t++) {
		file >> N >> K;
		powers2(N, K, binary);
	}

	file.close();
}


