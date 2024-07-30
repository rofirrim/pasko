#include "pasko_internal.h"
#include "pasko_runtime.h"

void __pasko_init(int argc, char *argv[], char *program_params[]) {
  (void)program_params;
  __pasko_init_io(argc, argv);
}
