int addOne(int g);

int one() {
    return 1;
}

int main() {    
    int a = one();
    return addOne(a) + a;
}

int addOne(int g) { 
    return 1 + g;
}
