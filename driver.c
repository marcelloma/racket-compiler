#include <stdio.h>
#include "entry.h"

#define FIXNUM_MASK 3
#define FIXNUM_TAG 0
#define FIXNUM_SHIFT 2

#define BOOLEAN_MASK 127
#define BOOLEAN_TAG 31
#define BOOLEAN_SHIFT 7

#define CHAR_MASK 255
#define CHAR_TAG 15
#define CHAR_SHIFT 8

#define EMPTY_LIST 47

int main(int argc, char** argv) {
  int val = entry();

  if ((val & BOOLEAN_MASK) == BOOLEAN_TAG) {
    printf("Boolean: %s\n", (val >> BOOLEAN_SHIFT) ? "true" : "false");
  } else if ((val & CHAR_MASK) == CHAR_TAG) {
    printf("Char: %c\n", val >> CHAR_SHIFT);
  } else if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("Fixnum: %d\n", val >> FIXNUM_SHIFT);
  } else if (val == EMPTY_LIST) {
    printf("Null\n");
  }

  return 0;
}
