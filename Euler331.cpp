#include <bits/stdc++.h>

using namespace std;

using ll = long long;

// Can approximate the result by taking the product of
// # cols w/ even # of 1s and # cols w/ odd # of 1s.
// This is correct for all empty squares, but wrong for squares
// w/ 1s. To correct it, go through every one individually
// and +1 or -1 depending on if it's surrounded by an even/odd #.

ll calcBoard2(ll n) {

    ll num_sqrs = 0;
    ll odd_cols = 0;

    ll y = n;

    // Go through each square in the solution, and check
    // if it's in an odd row/ column.
    for (ll x = 0; x < n; x++) {

        // (n-1)^2 - x^2 <= y^2
        //  y^2 < n^2 + x^2

        // y will always be reducing
        // Reduce it until the first valid square
        ll r = n*n - x*x;
        while (y*y >= r) { y--; }

        ll ys = y;

        r = n*n - 2*n + 1 - x*x;
        while ((y-1)*(y-1) >= r && y > 0) { y--; }

        bool oddY = (ys - y + 1) % 2 == 1;
        if (oddY) {
            odd_cols += 1;
        }

        for (ll y_cur = y; y_cur <= ys; y_cur++) {
            // Here, we go through every highlighted coordinate

            // Figure out the start/ end in the x direction, too.
            ll xs = x;
            r = n*n - y_cur*y_cur; // larger
            while ((xs+1)*(xs+1) < r) { xs++; }

            ll xe = x;

            r = n*n - 2*n + 1 - y_cur*y_cur;
            while ((xe-1)*(xe-1) >= r && xe > 0) { xe--; }

            bool oddX = (xs - xe + 1) % 2 == 1;

            num_sqrs += (oddX == oddY) * 2 - 1;

            // if (oddX == oddY) {
            //     num_sqrs += 1;
            // }
            // else  {
            //     num_sqrs -= 1;
            // }
        }
    }

    num_sqrs += 2 * odd_cols * (n - odd_cols);
    return num_sqrs;
}




int main() {
    // Only solvable odd board is 5x5 -> 3
    ll ans = 3;

    for (int i = 4; i <= 30; i += 2) {
        ll d = (1 << i) - i;
        ll t = calcBoard2(d);
        cout << i << ": " << t << endl;
        ans += t;
    }

    cout << ans << endl;
}
