#include <stdio.h>

extern int foo(int);

int main() {
  printf("%d\n", foo(2));
  return 0;
}