int algorythm(int n);
int calc_a(int n);
int calc_b(int n);
int getMax(int x, int y);

int main() {
    int n = 3;
    return algorythm(n);
}

int algorythm(int n) {
    // Only accept natural numbers
    if (n < 1) {
        return 0;
    }
    // Calculate 2 chunks of result
    int a = calc_a(n);
    int b = calc_b(n);

    // Return max chunk or 1st if equal
    return getMax(a, b);
}

int calc_a(int n) {
    int result = -2;
    int a1 = 2;
    int a2 = 3;
    bool cnt = true;

    // 'a' part calc
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

int calc_b(int n) {
    int result = 1;
    int b1 = 4;

    // 'b' part calc
    for (int i = 2; i < n + 1; i += 1) { 
        if (b1 % i == 0) {
            result *= b1 / i;
        }
        b1 *= 2;
    }
    return result;
}

int getMax(int x, int y) {
    if (x < y) {
        return y;
    }
    return x;
}
