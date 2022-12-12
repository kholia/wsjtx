set (libm65_FSRCS
# Modules come first:
  wideband_sync.f90

# Non-module Fortran routines:
  astro.f90
  astro0.f90
  astrosub.f90
  chkhist.f90
  chkmsg.f90
  dcoord.f90
  decode0.f90
  dot.f90
  fchisq0.f90
  filbig.f90
  four2a.f90
  ftninit.f90
  ftnquit.f90
  q65b.f90
  geocentric.f90
  graycode.f90
  grid2deg.f90
  indexx.f90
  interleave63.f90
  iqcal.f90
  iqfix.f90
  lorentzian.f90
  m65c.f90
  moon2.f90
  moondop.f90
  q65wa.f90
  recvpkt.f90
  sun.f90
  symspec.f90
  timf2.f90
  tm2.f90
  toxyz.f90

  f77_wisdom.f
  )

set (libm65_CXXSRCS
  ipcomm.cpp
  )

set (libm65_C_and_CXXSRCS
  ${libm65_CSRCS}
  ${libm65_CXXSRCS}
  )
set_property (SOURCE ${libm65_C_and_CXXSRCS} APPEND_STRING PROPERTY COMPILE_FLAGS " -include wsjtx_config.h")
set_property (SOURCE ${libm65_C_and_CXXSRCS} APPEND PROPERTY OBJECT_DEPENDS ${CMAKE_BINARY_DIR}/wsjtx_config.h)


#
# build our targets
#
add_library (m65impl STATIC ${libm65_FSRCS} ${libm65_CSRCS} ${libm65_CXXSRCS})
target_link_libraries (m65impl wsjt_fort wsjt_cxx Qt5::Core)

