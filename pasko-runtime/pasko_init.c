#include "pasko_internal.h"
#include "pasko_runtime.h"

void __pasko_init(int argc, char *argv[], int num_program_params,
                  char *program_params[], int num_global_files,
                  pasko_file_t **global_files[]) {
  (void)num_program_params;
  (void)program_params;
  (void)num_global_files;
  (void)global_files;
  __pasko_init_io(argc, argv);
}
