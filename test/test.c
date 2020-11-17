// int fib(int n);

// int main() {
//     int f = fib(10);
//     return f;
// }

// int fib(int n) {
//     int i = 0;
//     int j = 1;
//     int d = 0;
//     int c = i + j;

//     for (;; i += 1) {
//         if (i < n - 2) {
//             d = j + c;
//             j = c;
//             c = d;
//             continue;
//             c += 2;
//         } else {
//             break;
//         }
//     }

//     return d;
// }
int fact(int n) {
    int c = 1;
    for (int i = 2;;) {
        if (!(i < n - 1)) {
            c *= i;
            i += 1;
        } else {
            break;
        }
        continue;
        i *= 100;
    }
    return c;
}

int main() {
    return fact(10);
}
             