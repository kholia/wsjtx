#include <stdio.h>
#include "rs_sf.h"

static void *rs_sf;
static int first=1;
static int nn,kk,nroots,npad;

void rs_init_sf_(int *mm, int *nq, int *nn0, int *kk0, int *nfz)
// Initialize the RS decoder.
{
  // Save parameters nn, kk, nroots, npad for global access
  nn=*nn0;
  kk=*kk0;
  nroots=nn-kk;
  npad=*nq-1-nn;

  int gfpoly=0x43;    //For *mm=6
  if(*mm==7) gfpoly=0x89;
  if(*mm==8) gfpoly=0x11d;
  rs_sf=init_rs_sf(*mm,gfpoly,*nfz,1,nroots,npad);
  first=0;
}

void rs_encode_sf_(int *dgen, int *sent)
// Encode the information symbols dgen[KK], producing channel symbols sent[NN].
{
  int b[256];                    //These are the parity symbols
  encode_rs_sf(rs_sf,dgen,b);    //Compute the parity symbols

// Copy parity symbols into sent[] array, followed by information symbols
  for (int i=0; i< nn; i++) {
    if(i<nroots) {
      sent[i]=b[i];
    } else {
      sent[i]=dgen[i-nroots];
    }
  }
}

void rs_decode_sf_(int *recd, int *era_pos, int *numera, int *decoded,
		   int *nerr)
/*
Decode received data recd[NN], producing decoded[KK]. Positiions of 
erased symbols are specified in array era_pos[numera]. The number of 
corrected errors is *nerr; if the data are uncorrectable, *nerr=-1 
is returned.
*/
{
  *nerr=decode_rs_sf(rs_sf,recd,era_pos,*numera);
  for(int i=0; i<kk; i++) {
    decoded[i]=recd[nroots+i];
  }
}
