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

void rs_encode_sf_(int *msg, int *parsym)
// Encode information symbols msg[KK], producing parity symbols parsym[nroots].
{
  encode_rs_sf(rs_sf,msg,parsym);    //Compute the parity symbols
}

void rs_decode_sf_(int *recd, int *era_pos, int *numera, int *nerr)
{
  *nerr=decode_rs_sf(rs_sf,recd,era_pos,*numera);
}