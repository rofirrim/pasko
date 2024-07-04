{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids --must-fail-semantic %s 2>&1 | FileCheck %s
}

program test;
type
  pinteger = ^my_integer;

{
CHECK: error: type-identifier 'my_integer' has not been defined
}

begin
end.

