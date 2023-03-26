; ModuleID = 'output.bc'
source_filename = "mymod"

define void @main() {
entry:
  %i = alloca i32, align 4
  %ip = alloca ptr, align 8
  %sum = alloca i32, align 4
  store i32 9, ptr %i, align 4
  store ptr %i, ptr %ip, align 8
  %0 = load i32, ptr %i, align 4
  store i32 17, ptr %sum, align 4
}

define void @poop() {
entry:
}
