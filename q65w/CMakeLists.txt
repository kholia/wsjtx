set (q65w_CXXSRCS
  about.cpp
  astro.cpp
  devsetup.cpp
  displaytext.cpp
  getfile.cpp
  main.cpp
  mainwindow.cpp
  meterwidget.cpp
  plotter.cpp
  signalmeter.cpp
  soundin.cpp
  widegraph.cpp
  )

if (WIN32)
  set (q65w_CXXSRCS ${q65w_CXXSRCS} killbyname.cpp)
endif (WIN32)

set (q65w_UISRCS
  about.ui
  astro.ui
  devsetup.ui
  mainwindow.ui
  widegraph.ui
)

set (q65w_C_and_CXXSRCS
  ${q65w_CSRCS}
  ${q65w_CXXSRCS}
  )
set_property (SOURCE ${q65w_C_and_CXXSRCS} APPEND_STRING PROPERTY COMPILE_FLAGS " -include wsjtx_config.h")
set_property (SOURCE ${q65w_C_and_CXXSRCS} APPEND PROPERTY OBJECT_DEPENDS ${CMAKE_BINARY_DIR}/wsjtx_config.h)

# build the subdirectories
add_subdirectory (libm65)

# UI generation
qt5_wrap_ui (q65w_GENUISRCS ${q65w_UISRCS})

add_executable (q65w ${q65w_CXXSRCS} ${q65w_CSRCS} ${q65w_GENUISRCS} q65w.rc)
target_include_directories (q65w PRIVATE ${CMAKE_SOURCE_DIR} ${FFTW3_INCLUDE_DIRS})
target_link_libraries (q65w wsjt_qt m65impl ${FFTW3_LIBRARIES} Qt5::Widgets Qt5::Network Portaudio::Portaudio Usb::Usb)

if (WSJT_CREATE_WINMAIN)
  set_target_properties (q65w PROPERTIES WIN32_EXECUTABLE ON)
endif (WSJT_CREATE_WINMAIN)

if (WIN32)
  install (
    CODE "get_filename_component (_path \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/wsjtx_dir.txt\" REALPATH)
  if (WIN32)
    set (_separator \"\\\\\")
  else ()
    set (_separator \"/\")
  endif ()
  file (WRITE \"\${_path}\" \".\${_separator}\\n\")"
    )

  install (
    TARGETS q65w
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT runtime
    BUNDLE DESTINATION . COMPONENT runtime
    )
endif ()
