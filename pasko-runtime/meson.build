project('pasko-runtime', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

# These arguments are only used to build the shared library
# not the executables that use the library.
lib_args = ['-DBUILDING_PASKO_RUNTIME']

pasko_sources = [
  'pasko_error.c',
  'pasko_mem.c',
  'pasko_io.c',
  'pasko_string.c',
  # Headers
  'pasko_runtime.h',
  'pasko_internal.h',
]

shlib = shared_library('pasko_runtime', 
  pasko_sources,
  install : true,
  c_args : lib_args,
  gnu_symbol_visibility : 'hidden',
)

# Make this library usable as a Meson subproject.
pasko_runtime_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with : shlib)

# Make this library usable from the system's
# package manager.
install_headers('pasko_runtime.h', subdir : 'pasko_runtime')

pkg_mod = import('pkgconfig')
pkg_mod.generate(
  name : 'pasko-runtime',
  filebase : 'pasko_runtime',
  description : 'Runtime for Pasko',
  subdirs : 'pasko_runtime',
  libraries : shlib,
  version : '0.1',
)
