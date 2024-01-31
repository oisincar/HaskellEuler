#include <bits/stdc++.h>

using namespace std;

//   0
// +----+
// |1   |2
// +--3-+
// |4   |5
// +--6-+

int lookup[10] = {
  //6543210
  0b1110111, // 0
  0b0100100, // 1
  0b1011101, // 2
  0b1101101, // 3
  0b0101110, // 4
  0b1101011, // 5
  0b1111011, // 6
  0b0100111, // 7
  0b1111111, // 8
  0b1101111, // 9
};

int count_bits(int b) {
    int c = 0;
    while (b > 0) {
        c += b & 1;
        b >>= 1;
    }
    return c;
}

map<tuple<int, int>, int> cache;
int display_savings(int num, int last_num) {
    tuple<int, int> k = {num, last_num};
    if (cache.count(k)) {
        return cache[k];
    }

    // NOTE: last_num will always be longer or equal to num in length
    // except on the first iteration, when last_num will be 0.
    int cur_num = num;
    int savings = 0;
    int root = 0;

    while (cur_num > 0) {
        int new_disp = lookup[cur_num % 10];
        root += cur_num % 10;
        int old_disp = lookup[last_num % 10];

        // If last_num is 0, then no savings.
        if (last_num > 0) {
            // Sam's has to turn off old_disp, and turn on new_disp
            // Max's has to toggle difference between old and new.
            // Savings is 2x bits in common (these would be turned off then on)
            savings += 2 * count_bits(new_disp & old_disp);
        }

        cur_num /= 10;
        last_num /= 10;
    }

    if (num >= 10) {
        savings += display_savings(root, num);
    }

    cache[k] = savings;

    return savings;
}


int main() {
    int MIN_P = 10000000;
    int MAX_P = 20000000;

    vector<bool> is_prime(MAX_P, true);
    vector<int> lg_primes;

    // 0, 1 are not prime
    is_prime[0] = is_prime[1] = false;

    for (int i = 2; i < MAX_P; i++) {
        if (is_prime[i]) {

            if (i > MIN_P) {
                lg_primes.push_back(i);
            }

            // Remove multiples from is_prime arr.
            for (int m = 2; m * i < MAX_P; m++) {
                is_prime[m*i] = false;
            }
        }
    }

    long savings = 0;
    for (auto p : lg_primes) {
        savings += display_savings(p, 0);
    }

    cout << savings << endl;
}
