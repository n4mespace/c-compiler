int main() {
    int a = 1;
    {
        a = 2;
        {
            a = 3;
            if (a) {
                a = 4;
            }
            return a;
        }
    }
}
