; ModuleID = 'output.bc'
source_filename = "mymod"

define void @main() {
entry:
  %f = alloca float, align 4
  %ff = alloca double, align 8
  %fp = alloca ptr, align 8
  %sum = alloca double, align 8
  store float 8.000000e+00, ptr %f, align 4
  store double 9.000000e+00, ptr %ff, align 8
  store ptr %f, ptr %fp, align 8
  %0 = load ptr, ptr %fp, align 8
  %1 = load float, ptr %0, align 4
  %2 = load double, ptr %ff, align 8
  %3 = fptrunc double %2 to float
  %4 = fadd float %1, %3
  %5 = fpext float %4 to double
  store double %5, ptr %sum, align 8
}

define void @poop() {
entry:
}
