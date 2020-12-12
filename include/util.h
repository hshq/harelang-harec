#ifndef HARE_UTIL_H
#define HARE_UTIL_H

#define DJB2_INIT 5381

unsigned long djb2(unsigned long hash, char c);
unsigned long djb2_s(unsigned long hash, const char *str);

#endif
