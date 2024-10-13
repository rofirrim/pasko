{
RUN: %pasko --mode=ast-dump --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;

label 1000;

procedure foo;
begin
  goto 1000;
{
CHECK: non-local jump not allowed
}
end;

begin
1000: ;
end.
