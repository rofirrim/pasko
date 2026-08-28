{
RUN: %pasko --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;

procedure foo(x: integer); forward;

procedure foo;
var
  x : integer;
{ 
CHECK: identifier 'x' has already been declared in this scope
}
begin
end;

begin
end.
