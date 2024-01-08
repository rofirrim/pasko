{
RUN: %pasko --must-fail-parse --mode=ast-dump-pre --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program main;
var
  x : integer;

const 
  a = 3;

begin
end;

{
CHECK: error: unexpected token const, expecting one of "begin", "function", "identifier", "procedure"
}
