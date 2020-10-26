int main() {
    int b = 13 % 2;
    int a = 3;
    a = a + b;
    return ~(-a % b);
}
