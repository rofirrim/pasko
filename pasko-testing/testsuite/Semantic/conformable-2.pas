{
RUN: %pasko --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main(output);

procedure foo2(z: array[u0..l0 : integer] of integer);
begin
end;

procedure foo(y: array[u0..l0 : integer] of integer);
begin
  foo2(y);
{
CHECK: error: a conformable array cannot be passed to the value conformable array parameter z
}
end;

begin
end.

{


}
