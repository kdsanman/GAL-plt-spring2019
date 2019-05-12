; ModuleID = 'MicroC'
source_filename = "MicroC"

%struct.list = type { i32, %struct.list_node* }
%struct.list_node = type { i8*, %struct.list_node* }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@string = private unnamed_addr constant [6 x i8] c"hhelo\00", align 1
@string.3 = private unnamed_addr constant [2 x i8] c"x\00", align 1
@string.4 = private unnamed_addr constant [2 x i8] c"y\00", align 1
@string.5 = private unnamed_addr constant [2 x i8] c"z\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

declare %struct.list* @printil(%struct.list*)

declare %struct.list* @printl(%struct.list*)

declare i8* @string_concat(i8*, i8*)

declare i32 @str_size(i8*)

declare %struct.list* @make_list()

declare i32 @add_tail(%struct.list*, i8*)

define i32 @main() {
entry:
  %__l = alloca i32
  %__strlist = alloca %struct.list*
  %__intlist = alloca %struct.list*
  %__i = alloca i32
  %b = alloca i8*
  %c = alloca i8*
  %a = alloca %struct.list*
  %d = alloca %struct.list*
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @string, i32 0, i32 0), i8** %b
  %make_list = call %struct.list* @make_list()
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %data = bitcast i8* %malloccall to i32*
  store i32 1, i32* %data
  %data1 = bitcast i32* %data to i8*
  %list_add_tail = call i32 @add_tail(%struct.list* %make_list, i8* %data1)
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %data3 = bitcast i8* %malloccall2 to i32*
  store i32 2, i32* %data3
  %data4 = bitcast i32* %data3 to i8*
  %list_add_tail5 = call i32 @add_tail(%struct.list* %make_list, i8* %data4)
  store %struct.list* %make_list, %struct.list** %a
  %make_list6 = call %struct.list* @make_list()
  %malloccall7 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %data8 = bitcast i8* %malloccall7 to i8**
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string.3, i32 0, i32 0), i8** %data8
  %data9 = bitcast i8** %data8 to i8*
  %list_add_tail10 = call i32 @add_tail(%struct.list* %make_list6, i8* %data9)
  %malloccall11 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %data12 = bitcast i8* %malloccall11 to i8**
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string.4, i32 0, i32 0), i8** %data12
  %data13 = bitcast i8** %data12 to i8*
  %list_add_tail14 = call i32 @add_tail(%struct.list* %make_list6, i8* %data13)
  %malloccall15 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %data16 = bitcast i8* %malloccall15 to i8**
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string.5, i32 0, i32 0), i8** %data16
  %data17 = bitcast i8** %data16 to i8*
  %list_add_tail18 = call i32 @add_tail(%struct.list* %make_list6, i8* %data17)
  store %struct.list* %make_list6, %struct.list** %d
  %b19 = load i8*, i8** %b
  %"print f" = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i8* %b19)
  %a20 = load %struct.list*, %struct.list** %a
  %printil = call %struct.list* @printil(%struct.list* %a20)
  %d21 = load %struct.list*, %struct.list** %d
  %printl = call %struct.list* @printl(%struct.list* %d21)
  ret i32 0
}

declare noalias i8* @malloc(i32)
