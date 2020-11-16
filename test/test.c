int fib(int n);

int main() {
    int f = fib(10);
    return f;
}

int fib(int n) {
    int i = 0;
    int j = 1;
    int d = 0;
    int c = i + j;

    for (; i < n - 2; i += 1) {
        d = j + c;
        j = c;
        c = d;
        continue;
    }

    return d;
}