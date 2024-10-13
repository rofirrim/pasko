{
RUN: %pasko --mode=ast-dump --must-fail-semantic %s 2>&1 | FileCheck %s
}

program main;

label 1000;

{
CHECK: label has not been used
}

begin
end.
