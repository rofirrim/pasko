{
RUN: %pasko --must-fail-semantic %s 2>&1 | FileCheck %s
}
program test;

procedure my_proc(x: integer); forward;

procedure my_proc(x:real);
{
CHECK: error: procedure definition is incompatible with a previous procedure declaration
}
begin
   x := x + 1.0;
end;

begin
end.

