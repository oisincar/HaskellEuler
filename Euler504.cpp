#include <bits/stdc++.h>

using namespace std;


int gcd(int a, int b) {
    if (b == 0) {
        return a;
    }

    return gcd(b, a % b);
}

// Count # lattice points strictly contained in a triangle
// with pts (0, 0) (0, a), (b, 0)
int count_pts(int a, int b) {
    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    // A = i + b/2 - 1
    // i = (2A - b)/2 + 1
    // rearange w/ inline boundary pts:
    return ((a-1)*(b-1)+1 - gcd(a, b)) / 2;
}

int main() {
    set<int> squares;

    // Can't have larger squares than this.
    for (int i = 0; i <= 201; i++) {
        squares.insert(i*i);
    }

    int count = 0;
    int m = 100;

    for (int a = 1; a <= m; a++) {
        for (int b = 1; b <= m; b++) {
            for (int c = 1; c <= m; c++) {
                for (int d = 1; d <= m; d++) {
                    // Sum of points fully in each section + points along axes.
                    // +3 to include center point, but remove end point of each axis.
                    int p = count_pts(a, b)
                        + count_pts(b, c)
                        + count_pts(c, d)
                        + count_pts(d, a)
                        + (a + b + c + d - 3);

                    if (squares.count(p)) {
                        count += 1;
                    }
                }
            }
        }
    }

    cout << count << endl;
}
