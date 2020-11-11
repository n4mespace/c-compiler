int add(int a, int b);

int two() {
    return 2;
}

int main() {    
    int a = two();
    return add(a * a, a * a) + a;
}

int add(int g, int t) { 
    return g - t;
}
