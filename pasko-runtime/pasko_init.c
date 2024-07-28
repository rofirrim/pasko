#include "pasko_runtime.h"
#include "pasko_internal.h"

void __pasko_init(int argc, char *argv[]) {
  __pasko_init_io(argc, argv);
}
