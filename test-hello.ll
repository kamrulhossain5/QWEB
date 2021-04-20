; ModuleID = 'QWEB'
source_filename = "QWEB"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@string = private unnamed_addr constant [12 x i8] c"Hello World\00", align 1

declare i32 @prints(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %prints = call i32 (i8*, ...) @prints(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @string, i32 0, i32 0))
  ret i32 0
}
