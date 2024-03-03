#include <bits/stdc++.h>

using namespace std;


using ll = long long;


const int dim = 4;
const int dim2 = dim*dim;

// 0 1 2
// 3 4 5
// 6 7 8

int gcd(int a, int b) {
    if (b == 0) {
        return a;
    }

    return gcd(b, a % b);
}

// available_spots is a bitset
map<tuple<int, int>, ll> cache;

ll dfs(int ix, int available_spots) {
    tuple<int, int> k = {ix, available_spots};
    if (cache.count(k)) {
        return cache[k];
    }

    // Remove this from available
    available_spots &= ~(1 << ix);

    ll paths = 1; // Path can terminate here.

    for (int i = 0; i < dim2; i++) {
        int mask = (1 << i);
        if (!(available_spots & (1 << i))) {
            continue;
        }

        // Work out if this spot is blocked by another
        // spot.
        int x1 = ix % dim;
        int y1 = ix / dim;

        int x2 = i % dim;
        int y2 = i / dim;

        int dx = x2 - x1;
        int dy = y2 - y1;

        int d = abs(gcd(dx, dy));

        dx /= d;
        dy /= d;

        // Check up to but not including the spot.
        bool can_reach = true;
        for (int a = 1; a < d; a++) {
            int x_check = x1 + a*dx;
            int y_check = y1 + a*dy;

            // cout << ix << " " << i << " " << x1 << " " << y1 << " " << x2 << " " << y2 << " -- " << x_check << " " << y_check << endl;

            int ix_check = x_check + y_check*dim;

            // Spot in the middle is available
            if (available_spots & (1 << ix_check)) {
                can_reach = false;
                break;
            }
        }

        if (can_reach) {
            paths += dfs(i, available_spots & (~mask));
        }
    }

    cache[k] = paths;

    return paths;
}


int main() {
    int spots = (1 << (dim2+1)) - 1;

    ll ans = 0;
    for (int i = 0; i < dim2; i++) {
        ans += dfs(i, spots);
    }

    // Must have at least 2 squares to start!
    ans -= dim2;

    cout << ans << endl;
}
