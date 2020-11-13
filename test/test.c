int main() {
    int f = 1;
    for (int i = 1; i < 10; i += 1) {
        f += i;
    }
    while (f > 15) {
        f -= 1;
    }
    for (;;) {
        f /= 2;
    }

    return f;
}
