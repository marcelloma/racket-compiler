#include <stdio.h>
#include "entry.h"

int main(int argc, char** argv) {
  int val = entry();

  printf("%d\n", val);

  return 0;
}
