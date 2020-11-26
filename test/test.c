int one();

int main() {
    int a = one();
    a %= 4;
    return a;
}

int one() {
    return 1;
}
