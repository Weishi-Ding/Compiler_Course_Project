; ModuleID = 'MicroJ'
source_filename = "MicroJ"

@my_struct_ptr = global { i64*, i64, i64 }* null
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str = private unnamed_addr constant [7 x i8] c"\22Full\22\00", align 1
@str.4 = private unnamed_addr constant [8 x i8] c"\22Empty\22\00", align 1
@fmt.5 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.6 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.7 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.8 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.9 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.10 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.11 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.12 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.13 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.14 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.15 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.16 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.17 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.18 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.19 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.20 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.21 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.22 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(i8*, ...)

define { i64*, i64, i64 }* @Stack() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i64*, i64, i64 }* getelementptr ({ i64*, i64, i64 }, { i64*, i64, i64 }* null, i32 1) to i32))
  %struct_ptr = bitcast i8* %malloccall to { i64*, i64, i64 }*
  store { i64*, i64, i64 }* %struct_ptr, { i64*, i64, i64 }** @my_struct_ptr, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 0
  store i64* null, i64** %field_ptr, align 8
  %field_ptr1 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 1
  store i64 0, i64* %field_ptr1, align 4
  %field_ptr2 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 2
  store i64 0, i64* %field_ptr2, align 4
  ret { i64*, i64, i64 }* %struct_ptr
}

define { i64*, i64, i64 }* @Stack.1(i64 %0) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i64*, i64, i64 }* getelementptr ({ i64*, i64, i64 }, { i64*, i64, i64 }* null, i32 1) to i32))
  %struct_ptr = bitcast i8* %malloccall to { i64*, i64, i64 }*
  store { i64*, i64, i64 }* %struct_ptr, { i64*, i64, i64 }** @my_struct_ptr, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 0
  store i64* null, i64** %field_ptr, align 8
  %field_ptr1 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 1
  store i64 0, i64* %field_ptr1, align 4
  %field_ptr2 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 2
  store i64 0, i64* %field_ptr2, align 4
  %field_ptr3 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %struct_ptr, i32 0, i32 1
  store i64 %0, i64* %field_ptr3, align 4
  ret { i64*, i64, i64 }* %struct_ptr
}

define void @init({ i64*, i64, i64 }* %self) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %self2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self2, i32 0, i32 0
  %self3 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr4 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self3, i32 0, i32 1
  %0 = load i64, i64* %field_ptr4, align 4
  %1 = trunc i64 %0 to i32
  %mallocsize = mul i32 %1, ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32)
  %malloccall = tail call i8* @malloc(i32 %mallocsize)
  %array = bitcast i8* %malloccall to i64*
  store i64* %array, i64** %field_ptr, align 8
  ret void
}

define i1 @push({ i64*, i64, i64 }* %self, i64 %val) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %val2 = alloca i64, align 8
  store i64 %val, i64* %val2, align 4
  %self3 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %isFull_result = call i1 @isFull({ i64*, i64, i64 }* %self3)
  br i1 %isFull_result, label %then, label %else

merge:                                            ; preds = %else
  %dataTemp = alloca i64*, align 8
  %self4 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self4, i32 0, i32 0
  %0 = load i64*, i64** %field_ptr, align 8
  store i64* %0, i64** %dataTemp, align 8
  %self5 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr6 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self5, i32 0, i32 2
  %1 = load i64, i64* %field_ptr6, align 4
  %dataTemp7 = load i64*, i64** %dataTemp, align 8
  %array = getelementptr i64, i64* %dataTemp7, i64 %1
  %val8 = load i64, i64* %val2, align 4
  store i64 %val8, i64* %array, align 4
  %self9 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr10 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self9, i32 0, i32 2
  %self11 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr12 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self11, i32 0, i32 2
  %2 = load i64, i64* %field_ptr12, align 4
  %tmp = add i64 %2, 1
  store i64 %tmp, i64* %field_ptr10, align 4
  ret i1 true

then:                                             ; preds = %entry
  ret i1 false

else:                                             ; preds = %entry
  br label %merge
}

define i64 @pop({ i64*, i64, i64 }* %self) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %self2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %isEmpty_result = call i1 @isEmpty({ i64*, i64, i64 }* %self2)
  br i1 %isEmpty_result, label %then, label %else

merge:                                            ; preds = %else
  %dataTemp = alloca i64*, align 8
  %self3 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self3, i32 0, i32 0
  %0 = load i64*, i64** %field_ptr, align 8
  store i64* %0, i64** %dataTemp, align 8
  %result = alloca i64, align 8
  %self4 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr5 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self4, i32 0, i32 2
  %1 = load i64, i64* %field_ptr5, align 4
  %tmp = sub i64 %1, 1
  %dataTemp6 = load i64*, i64** %dataTemp, align 8
  %array = getelementptr i64, i64* %dataTemp6, i64 %tmp
  %dataTemp7 = load i64, i64* %array, align 4
  store i64 %dataTemp7, i64* %result, align 4
  %self8 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr9 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self8, i32 0, i32 2
  %self10 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr11 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self10, i32 0, i32 2
  %2 = load i64, i64* %field_ptr11, align 4
  %tmp12 = sub i64 %2, 1
  store i64 %tmp12, i64* %field_ptr9, align 4
  %result13 = load i64, i64* %result, align 4
  ret i64 %result13

then:                                             ; preds = %entry
  ret i64 -1

else:                                             ; preds = %entry
  br label %merge
}

define i64 @peak({ i64*, i64, i64 }* %self) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %dataTemp = alloca i64*, align 8
  %self2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self2, i32 0, i32 0
  %0 = load i64*, i64** %field_ptr, align 8
  store i64* %0, i64** %dataTemp, align 8
  %self3 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr4 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self3, i32 0, i32 2
  %1 = load i64, i64* %field_ptr4, align 4
  %tmp = sub i64 %1, 1
  %dataTemp5 = load i64*, i64** %dataTemp, align 8
  %array = getelementptr i64, i64* %dataTemp5, i64 %tmp
  %dataTemp6 = load i64, i64* %array, align 4
  ret i64 %dataTemp6
}

define i1 @isEmpty({ i64*, i64, i64 }* %self) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %self2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self2, i32 0, i32 2
  %0 = load i64, i64* %field_ptr, align 4
  %tmp = icmp eq i64 %0, 0
  ret i1 %tmp
}

define i1 @isFull({ i64*, i64, i64 }* %self) {
entry:
  %self1 = alloca { i64*, i64, i64 }*, align 8
  store { i64*, i64, i64 }* %self, { i64*, i64, i64 }** %self1, align 8
  %self2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self2, i32 0, i32 2
  %0 = load i64, i64* %field_ptr, align 4
  %self3 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %self1, align 8
  %field_ptr4 = getelementptr inbounds { i64*, i64, i64 }, { i64*, i64, i64 }* %self3, i32 0, i32 1
  %1 = load i64, i64* %field_ptr4, align 4
  %tmp = icmp eq i64 %0, %1
  ret i1 %tmp
}

declare noalias i8* @malloc(i32)

define i64 @main() {
entry:
  %s = alloca { i64*, i64, i64 }*, align 8
  %Stack_result = call { i64*, i64, i64 }* @Stack.1(i64 10)
  store { i64*, i64, i64 }* %Stack_result, { i64*, i64, i64 }** %s, align 8
  %s1 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %s, align 8
  call void @init({ i64*, i64, i64 }* %s1)
  %i = alloca i64, align 8
  store i64 0, i64* %i, align 4
  store i64 0, i64* %i, align 4
  br label %while

while:                                            ; preds = %while_body, %entry
  %i5 = load i64, i64* %i, align 4
  %tmp6 = icmp slt i64 %i5, 10
  br i1 %tmp6, label %while_body, label %merge

while_body:                                       ; preds = %while
  %s2 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %s, align 8
  %i3 = load i64, i64* %i, align 4
  %push_result = call i1 @push({ i64*, i64, i64 }* %s2, i64 %i3)
  %i4 = load i64, i64* %i, align 4
  %tmp = add i64 %i4, 1
  store i64 %tmp, i64* %i, align 4
  br label %while

merge:                                            ; preds = %while
  %s7 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %s, align 8
  %isFull_result = call i1 @isFull({ i64*, i64, i64 }* %s7)
  br i1 %isFull_result, label %then, label %else

merge8:                                           ; preds = %else, %then
  store i64 0, i64* %i, align 4
  br label %while9

then:                                             ; preds = %merge
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i32 0, i32 0))
  br label %merge8

else:                                             ; preds = %merge
  br label %merge8

while9:                                           ; preds = %while_body10, %merge8
  %i16 = load i64, i64* %i, align 4
  %tmp17 = icmp slt i64 %i16, 10
  br i1 %tmp17, label %while_body10, label %merge11

while_body10:                                     ; preds = %while9
  %s12 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %s, align 8
  %pop_result = call i64 @pop({ i64*, i64, i64 }* %s12)
  %printf13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i64 %pop_result)
  %i14 = load i64, i64* %i, align 4
  %tmp15 = add i64 %i14, 1
  store i64 %tmp15, i64* %i, align 4
  br label %while9

merge11:                                          ; preds = %while9
  %s18 = load { i64*, i64, i64 }*, { i64*, i64, i64 }** %s, align 8
  %isEmpty_result = call i1 @isEmpty({ i64*, i64, i64 }* %s18)
  br i1 %isEmpty_result, label %then20, label %else22

merge19:                                          ; preds = %else22, %then20
  ret i64 0

then20:                                           ; preds = %merge11
  %printf21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* @str.4, i32 0, i32 0))
  br label %merge19

else22:                                           ; preds = %merge11
  br label %merge19
}
