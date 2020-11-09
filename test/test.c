int one();

int two() {
    return 2;
}

int main() {    
    int a = one();
    a *= 3;
    return two() * a;
}

int one() {
    return 1;
}