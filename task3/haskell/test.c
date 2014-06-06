int a, b;

int calc(int c) {
  int d;
  d = (a + b) * c;
  return d;
}
int func() {
  a = -1;
  b = a;
  return calc(5);
}