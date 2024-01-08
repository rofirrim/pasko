{
RUN: %pasko --mode=ast-dump-pre --must-fail-parse --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

{ This is not properly handled at the moment }

program main;
begin
  ;;;
end.

{

CHECK: error: unexpected token ;,

}
