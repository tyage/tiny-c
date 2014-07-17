#include <stdio.h>

extern int test_calc(int, int);
extern int test_if(int);
extern int test_while(int);
extern int test_call(int);
extern int test_global();

int main() {
  printf("%d\n", test_calc(1, 1));
  printf("%d\n", test_calc(2, 2));
  printf("%d\n", test_if(1));
  printf("%d\n", test_if(2));
  printf("%d\n", test_while(1));
  printf("%d\n", test_while(2));
  printf("%d\n", test_call(1));
  printf("%d\n", test_call(2));
  printf("%d\n", test_global());
  printf("%d\n", test_global());
  return 0;
}
