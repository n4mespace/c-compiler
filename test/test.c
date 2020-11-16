int fib(int n);

int main() {
    int f = fib(10);
    return f;
}

int fib(int n) {
    int i = 0;
    int j = 1;
    int d = 0;
    int c = n;

    for (int i = 0; i < n - 2; i += 1) {
        d = j + c;
        j = c;
        c = d;
    }
 
    return d;
}

int main1() {
    int l = 2;
    int a = l;
    if (false) {
        int b = 13;
        {
            int l = 3;
            int c = 2 * b;
        }
        return b;
    } else {
        return 4;
    }
}
