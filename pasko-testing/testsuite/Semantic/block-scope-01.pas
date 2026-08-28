{
RUN: %pasko --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;

function foo(x: integer) : integer;
var
  x : integer;
{ 
CHECK: identifier 'x' has already been declared in this scope
}
begin
end;

begin
end.
