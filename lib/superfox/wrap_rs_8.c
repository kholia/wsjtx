#include <stdio.h>

void encode_rs_8_(unsigned char data[], unsigned char parity[], int *npad)
{
  encode_rs_8(data,parity,*npad);           //Compute the parity symbols
}


int decode_rs_8_(unsigned char *data, int *era_pos, int *numera, int *npad)
{
  int nerr;
  nerr=decode_rs_8(data,era_pos,*numera,*npad);
  return nerr;
}
