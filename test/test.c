int fib(int n);

int main() {
    int f = fib(10);
    return f;
}

int fib(int n) {
    int i = 0;
    int j = 1;
    int d = 0;
    int count = 0;
    int c = i + j;

    while(count < n - 2) {
        d = j + c;
        j = c;
        c = d;
        count += 1;
    }
    return d;
}
