int main() {
    bool flag = false;
    int cnt = 100;
    if (!flag) {
        int b = 13;
        {
          int c = 2 * b;
        }
        return b; 
    } else {
        return cnt / 4;
    }
}