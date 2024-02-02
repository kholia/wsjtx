#include <stdio.h>
#include "rs_sf.h"

void *rs_sf;
static int first=1;
static int nn,kk,nroots,npad;

void rs_init_sf_(int *mm, int *nq, int *nn0, int *kk0, int *nfz)
{
  nn=*nn0;
  kk=*kk0;
  nroots=nn-kk;
  npad=*nq-1-nn;
  if(*mm==6) rs_sf=init_rs_sf(*mm,0x43,*nfz,1,nroots,npad);   //M=6
  if(*mm==7) rs_sf=init_rs_sf(*mm,0x89,*nfz,1,nroots,npad);   //M=7
  if(*mm==8) rs_sf=init_rs_sf(*mm,0x11d,*nfz,1,nroots,npad);  //M=8
  first=0;
}

void rs_encode_sf_(int *dgen, int *sent)
     // Encode JT65 data dgen[...], producing sent[...].
{
  int dat1[256];
  int b[256];
  int i;

  for(i=0; i<kk; i++) {          //Copy data into dat1
    dat1[i]=dgen[i];
  }

  encode_rs_sf(rs_sf,dat1,b);    // Compute the parity symbols

  // Copy parity symbols into sent[] array, followed by data
  for (i = 0; i < nroots; i++) {
    sent[i] = b[i];
  }

  for (i = 0; i < kk; i++) {
    sent[i+nroots] = dat1[i];
  }
}

void rs_decode_sf_(int *recd, int *era_pos, int *numera, int *decoded, int *nerr)
     // Decode JT65 received data recd0[63], producing decoded[12].
     // Erasures are indicated in era0[numera].  The number of corrected
     // errors is *nerr.  If the data are uncorrectable, *nerr=-1 is
     // returned.
{
  int i;
  *nerr=decode_rs_sf(rs_sf,recd,era_pos,*numera);
  for(i=0; i<kk; i++) {
    decoded[i]=recd[nroots+i];
  }
}
