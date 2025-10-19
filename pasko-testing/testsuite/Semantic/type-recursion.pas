{
RUN: %pasko --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;

type
  my_matrix = array[1..9] of array [10..19] of my_matrix;
{
CHECK: error: invalid cyclic reference to type being declared
}

procedure foo(var x: my_matrix);
begin
  x[1][10] := 42;
end;

begin
end.
