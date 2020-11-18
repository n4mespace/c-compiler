int main() {
    int outer = 1;
    int inner = outer;

    while (outer < 7) {
        for (; inner < 10; inner += 1) {
            inner *= outer;
            while (true) {
                break;
            }
        }
        outer += 1;

        continue;
        outer *= 100;
    }
    return outer + inner;
}
