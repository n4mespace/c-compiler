// Program to calculate max out of n-th member of
// -(2*1- 2*3- 4*3- 4*5- 6*5- 6*7-…) series and
// only whole numbers of 1/1* 4/2* 8/3* 16/4... series

// Declaration of additional functions
int algorithm(int n);
int series_a(int n);
int series_b(int n);
int getMax(int x, int y);

// Main function
// Return: algoritm result
int main() {
    int n = 10;
    return algorithm(n);
}

// n - series member number
// Return: max value of n-th member of 2 series
int algorithm(int n) {
    // Only accept natural numbers
    if (n < 1) {
        return 0;
    }
    // Calculate 2 series
    int a = series_a(n);
    int b = series_b(n);

    // Return max series member or 1st if equal
    return getMax(a, b);
}

// n - series member number
// Return: n-th member of series
int series_a(int n) {
    int result = -2;
    int a1 = 2;
    int a2 = 3;
    bool cnt = true;

    for (int i = 1; i < n; i += 1) {
        result += a1 * a2;
        if (cnt) {
            a1 += 2;
        } else {
            a2 += 2;
        }
        cnt = !cnt;
    }
    return result;
}

// n - series member number
// Return: n-th member of series
int series_b(int n) {
    int result = 1;
    int b1 = 4;

    for (int i = 2; i < n + 1; i += 1) { 
        if (b1 % i == 0) {
            result *= b1 / i;
        }
        b1 *= 2;
    }
    return result;
}

// x - number
// y - number
// Return: max(x, y)
int getMax(int x, int y) {
    if (x - y < 0) {
        return y;
    }
    return x;
}
