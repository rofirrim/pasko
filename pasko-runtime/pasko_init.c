#include "pasko_internal.h"
#include "pasko_runtime.h"
#include <stdio.h>

void __pasko_init(int argc, char *argv[], int num_program_params,
                  char *program_params[], int num_global_files,
                  pasko_file_t **global_files[]) {
  __pasko_init_io(argc, argv, num_program_params, program_params,
                  num_global_files, global_files);
}

void __pasko_finish(int num_global_files, pasko_file_t **global_files[]) {
  __pasko_finish_io(num_global_files, global_files);
}

void __pasko_ignoring_argument(const char *arg, const char *reason) {
  if (!reason)
    fprintf(stderr, "WARNING: ignoring argument '%s'", arg);
  else
    fprintf(stderr, "WARNING: ignoring argument '%s'. Reason: %s", arg, reason);
}
