// int addTwoIfFlag(int value, bool flag);
// int addOne(int value);

// int main() {
//     bool flag = true;
//     char ch = 'c';
//     int a = addOne(8);
//     int b = addTwoIfFlag(a * 2, flag);
//     return addTwoIfFlag(ch + 1, !flag);
// }

// int addOne(int v) {
//     return v + 1;
// }

// int addTwoIfFlag(int v, bool f) {
//     if (f) {
//         return v;
//     } else {
//         return v + 2;
//     }
// }

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