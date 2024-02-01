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

  // Reverse data order for the Karn codec.
  for(i=0; i<kk; i++) {
    dat1[i]=dgen[kk-1-i];
  }
  // Compute the parity symbols
  encode_rs_sf(rs_sf,dat1,b);

  // Move parity symbols and data into sent[] array, in reverse order.
  for (i = 0; i < nroots; i++) sent[nroots-1-i] = b[i];
  for (i = 0; i < kk; i++) sent[i+nroots] = dat1[kk-1-i];
}

void rs_decode_sf_(int *recd0, int *era0, int *numera0, int *decoded, int *nerr)
     // Decode JT65 received data recd0[63], producing decoded[12].
     // Erasures are indicated in era0[numera].  The number of corrected
     // errors is *nerr.  If the data are uncorrectable, *nerr=-1 is
     // returned.
{
  int numera;
  int i;
  int era_pos[200];
  int recd[255];

  numera=*numera0;
  for(i=0; i<kk; i++) recd[i]=recd0[nn-1-i];
  for(i=0; i<nroots; i++) recd[kk+i]=recd0[nroots-1-i];
  if(numera) 
    for(i=0; i<numera; i++) era_pos[i]=era0[i];
  *nerr=decode_rs_sf(rs_sf,recd,era_pos,numera);
  for(i=0; i<kk; i++) decoded[i]=recd[kk-1-i];
}
