{
RUN: %pasko --mode=parse-semantic %s 2>&1 | FileCheck %s
}
program main;

function foo0(x: integer): integer; forward;
function foo0(x: integer): integer; forward;
{
CHECK: function declaration is redundant
}

function foo1(x: integer): integer; forward;
function foo1(x: integer): integer;
{
CHECK: function definition is reintroducing parameter declarations
}
begin
   foo1 := x;
end;


procedure bar0(x: integer); forward;
procedure bar0(x: integer); forward;
{
CHECK: procedure declaration is redundant
}

procedure bar1(x: integer); forward;
procedure bar1(x: integer);
{
CHECK: procedure definition is reintroducing parameter declarations
}
begin
end;

begin
end.
