int factLoop(int n);
int factRecursion(int n);
             
int main() {
	int l = factLoop(10);
	int r = factRecursion(10);

	if (l == r) {
		return 0;
	}

	return 1;
}

int factLoop(int n) {
	int c = 1;
	for (int i = 2; i < n - 1;) {
		c *= i;
		i += 1;
	}
	return c;
}

int factRecursion(int n) {
	if (n < 1) {
		return 1;
	}
	return n * factRecursion(n - 1);
}
