int test();

int main() {    
    int a = test();
    test();
    a /= 3;
    return a;
}

int test() {
    return 1;
}