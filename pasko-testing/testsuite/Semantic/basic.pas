{
RUN: %pasko --ast-dump-no-ids --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;
const  
  KONST = 123;
  BLAH = true;
{
C-HECK: error: negation of a constant that is not an integer or a real
}
  BAR = -BLAH;
var
  x: integer;
  z: real;
  b: boolean;
{
CHECK: error: identifier 'mytype' not found in this scope
}
  c: mytype;
{
CHECK: error: identifier 'x' has not been declared as a type in this scope
}
  d: x;

function two_args(x, y : integer): integer;
begin
   two_args := 0;
end;

procedure foo(var z: integer);
var
  a: integer;
{
CHECK: error: identifier 'a' has already been declared in this scope
}
  a: real;
begin
end;

begin
{ This should not crash }
   x := BAR;
{
CHECK: error: identifier 'notfound' not found in this scope 
}
   notfound := 3;
{
CHECK: error: operator 'and' cannot be applied to operands of type 'integer' and 'integer'
}
   x := x and x;
{
CHECK: error: operator '-' cannot be applied to operand of type 'boolean
}
   b := -b;
{
CHECK: error: function 'two_args' expects 2 parameters but 1 arguments were passed
}
   x := two_args(1);
{
CHECK: error: argument has type real that is not assignment compatible with value parameter 'x' of type integer
}
   x := two_args(1.2, 3);
{
CHECK: error: argument is not a variable, as required by variable parameter 'z'
}
   foo(2);
{
CHECK: error: argument has type real but it is different to variable parameter 'z' of type integer
}
   foo(z);
{
CHECK: error: identifier 'konst' has not been declared as a variable in this scope
}
   KONST := 4;
{
CHECK: error: left-hand side of this assignment has type 'integer' that is not assignment-compatible with the type 'real' of the right-hand side
}
   x := z;
{
CHECK: error: the expression of an if-statement must be of boolean type
}
  if 3 then
  begin
  end;
{
CHECK: error: the expression of a while-statement must be of boolean type
}
  while 3 do
  begin
  end;
{
CHECK: error: the expression of a repeat-statement must be of boolean type
}
  repeat
  until 3;
{
CHECK: error: the control-variable of a for-statement must be of ordinal type 
}
  for z := 1 to 10 do 
  begin
  end;
{
CHECK: error: initial value expression is not assignment-compatible with the control-variable of the for-statement
CHECK: error: final value expression is not assignment-compatible with the control-variable of the for-statement
}
  for x := 1.2 to 10.0 do 
  begin
  end;
{
CHECK: error: required procedure 'new' cannot be referenced in a function call
}
   x := new(1);
{
CHECK: error: identifier 'z' has not been declared as a function in this scope and cannot be called
}
   x := z(1);
{
CHECK: error: identifier 'z' has not been declared as a procedure in this scope and cannot be called
}
   z(1);
{
CHECK: error: cannot call required function 'abs' like a procedure
}
   abs(3);
{
CHECK-NOT: error:
}
end.

