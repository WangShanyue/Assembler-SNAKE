    ;演示程序（引导程序）dp77.asm
    Signature  equ   0              ;工作程序签名所在位置偏移
    Length     equ   6              ;工作程序长度所在位置偏移
    Start      equ   8              ;工作程序启动位置所在偏移
    ZONELOW    equ   1000H          ;缺省的工作程序使用内存区域的段值
    ZONEHIGH   equ   9000H          ;工作程序使用的内存区域段值上限
    ZONETEMP   equ   07E0H          ;首扇区的缓冲区段值
        ;
        section   text              ;段text
        bits  16                    ;16位段模式
        org   7C00H                 ;自身的起始偏移
        ;
    Begin:
        MOV   AX, 0
        CLI
        MOV   SS, AX                ;设置堆栈//@1
        MOV   SP, 7C00H             ;把堆栈底安排在07C0:0000
        STI
        ;
    Lab1:                           ;循环加载的起点//@2
        CLD
        PUSH  CS
        POP   DS                    ;DS=CS，准备填写DAP
        MOV   AX, ZONETEMP          ;把临时内存区域的段值
        MOV   WORD [DiskAP+6], AX   ;填写到DAP中的缓冲区段值字段
        MOV   ES, AX                ;也保存到ES
        ;
        MOV   DX, mess0             ;提示输入的信息
        CALL  PutStr                ;提示用户输入工作程序起始扇区LBA
		;添加功能代码
	lable0:
		CALL GetChar
		CALl PutChar;
		cmp AL,30H;
		JZ    Over;      系统挂起程序
		cmp AL,31H;
		jne lable1;
		mov EAX,10;      指定中断处理程序在150号扇区
		jmp lable5;
	lable1:
		MOV   DX, mess4            ;提示输入的信息,用户重新输入
        CALL  PutStr
        jmp lable0;		
        CALL  GetSecAdr             ;接受用户的输入
        OR    EAX, EAX              ;如果用户输入为0，则转停止
        JZ    Over
    lable5:   ;---------------------------
        MOV   [DiskAP+8], EAX       ;填写到DAP中的扇区LBA低4字节字段
        CALL  ReadSec               ;读工作程序首扇区
        JC    Lab7                  ;读出错，则转
        ;---------------------------
        CMP   DWORD [ES:Signature], "YANG"      ;核查工作程序的签名
        JNZ   Lab6                  ;签名不正确，则转
        ;---------------------------
        MOV   CX, [ES:Length]       ;取得工作程序长度
        CMP   CX, 0                 ;长度不应该为0
        JZ    Lab6                  ;如果为0，作为签名不正确处理
        ADD   CX, 511               ;为便于计算需要读取的扇区数
        SHR   CX, 9                 ;相当于除512，得扇区数
        ;---------------------------
        MOV   AX, [ES:Start+2]      ;取得工作程序期望内存段值
        CMP   AX, ZONELOW           ;期望的内存区域必须在规定范围内
        JB    Lab2                  ;如超出范围，则取下限
        CMP   AX, ZONEHIGH
        JB    Lab3
    Lab2:
        MOV   AX, ZONELOW           ;如超出范围，则取下限
    Lab3:
        MOV   WORD [DiskAP+6], AX   ;设置DAP中的缓冲区段值
        ;---------------------------
        MOV   ES, AX                ;同时保存到ES
        XOR   DI, DI                ;准备复制已经在内存中的首个扇区
        PUSH  DS
        PUSH  ZONETEMP              ;首扇区的缓冲区段值
        POP   DS                    ;源段值
        XOR   SI, SI
        PUSH  CX                    ;CX含有工作程序的扇区数
        MOV   CX, 128
        REP   MOVSD                 ;复制128个双字
        POP   CX
        POP   DS
        ;---------------------------
        DEC   CX                    ;已经读取过一个扇区
        JZ    Lab5                  ;如工作程序只有一个扇区，转
    Lab4:
        ADD   WORD [DiskAP+6], 20H  ;调整缓冲区段值，即内存的下512字节位置
        INC   DWORD [DiskAP+8]      ;准备读取下一个扇区 
        CALL  ReadSec               ;读一个扇区
        JC    Lab7                  ;读出错，则转
        LOOP  Lab4                  ;还有，则继续
        ;---------------------------
    Lab5:
        MOV   [ES:Start+2], ES      ;设置工作程序入口点的段值
        CALL  FAR  [ES:Start]       ;调用工作程序//@3
        JMP   Lab1                  ;准备加载下一个工作程序
        ;---------------------------
    Lab6:
        MOV   DX, mess1             ;提示无效工作程序
        CALL  PutStr                ;给出提示信息
        JMP   Lab1                  ;准备引导下一个工作程序
    Lab7:
        MOV   DX, mess2             ;提示读磁盘出错
        CALL  PutStr                ;给出提示信息
        JMP   Lab1
    Over:
        MOV   DX, mess3             ;结束提示
        CALL  PutStr                ;提示挂起
    Halt:
        HLT
        JMP   SHORT  Halt           ;陷入无限循环
    ;===============================
    ReadSec:                        ;读1个指定的扇区到指定内存区域
        PUSH  DX
        PUSH  SI
        MOV	  SI, DiskAP            ;指向DAP（含扇区LBA和缓冲区地址）
        MOV	  DL, 80H               ;C盘
        MOV	  AH, 42H               ;扩展方式读
        INT   13H                   ;读！
        POP   SI
        POP   DX
        RET
    ;-------------------------------
    GetSecAdr:                      ;接受用户键盘输入工作程序所在扇区的LBA
        MOV   DX, buffer            ;DX指向缓冲区首
        CALL  GetDStr               ;接受用户输入一个数字串（回车结尾）
        MOV   AL, 0DH               ;形成回车换行效果
        CALL  PutChar
        MOV   AL, 0AH
        CALL  PutChar        
        MOV   SI, buffer+1          ;DX指向缓冲区中的数字串
        CALL  DSTOB                 ;将数字串转成对应的二进制值（至少返回零）
        RET
    ;-------------------------------
    DSTOB:                          ;将数字串转换成对应的二进制值
        XOR   EAX, EAX
        XOR   EDX, EDX
    .next:
        LODSB                       ;取一个数字符
        CMP   AL, 0DH
        JZ    .ok
        AND   AL, 0FH
        IMUL  EDX, 10
        ADD   EDX, EAX
        JMP   SHORT .next
    .ok:
        MOV   EAX, EDX              ;EAX返回二进制值
        RET
    ;-------------------------------
    %define  Space      20H         ;空格符
    %define  Enter      0DH         ;回车符
    %define  Backspace  08H         ;退格
    %define  Bell       07H         ;响铃
    ;子程序名：GetDStr
    ;功    能：接受一个由十进制数字符组成的字符串
    ;入口参数：DS:DX=缓冲区首地址
    ;说    明：（1）缓冲区第一个字节是其字符串容量
    ;          （2）返回的字符串以回车符（0DH）结尾
    GetDStr:
        PUSH  SI
        MOV   SI, DX
        MOV   CL, [SI]              ;取得缓冲区的字符串容量
        CMP   Cl, 1                 ;如小于1，直接返回
        JB    .Lab6
        ;
        INC   SI                    ;指向字符串的首地址
        XOR   CH, CH                ;CH作为字符串中的字符计数器，清零
    .Lab1:
        CALL  GetChar               ;读取一个字符
        OR    AL, AL                ;如为功能键，直接丢弃//@1
        JZ    SHORT  .Lab1
        CMP   AL, Enter             ;如为回车键，表示输入字符串结束
        JZ    SHORT  .Lab5          ;转输入结束
        CMP   AL,  Backspace        ;如为退格键
        JZ    SHORT  .Lab4          ;转退格处理
        CMP   AL, Space             ;如为其他不可显示字符，丢弃//@2
        JB    SHORT  .Lab1
        ;
        cmp   al, '0'
        jb    short  .Lab1          ;小于数字符，丢弃
        cmp   al, '9'
        ja    short  .Lab1          ;大于数字符，丢弃
        ;
        CMP   Cl, 1                 ;字符串中的空间是否有余？
        JA    SHORT  .Lab3          ;是，转存入字符串处理
    .Lab2:
        MOV   AL, Bell
        CALL  PutChar               ;响铃提醒
        JMP   SHORT  .Lab1          ;继续接受字符
        ;
    .Lab3:
        CALL  PutChar               ;显示字符
        MOV   [SI], AL              ;保存到字符串
        INC   SI                    ;调整字符串中的存放位置
        INC   CH                    ;调整字符串中的字符计数
        DEC   CL                    ;调整字符串中的空间计数
        JMP   SHORT  .Lab1          ;继续接受字符
        ;
    .Lab4:                          ;退格处理
        CMP   CH, 0                 ;字符串中是否有字符？
        JBE   .Lab2                 ;没有，响铃提醒
        CALL  PutChar               ;光标回退
        MOV   AL, Space
        CALL  PutChar               ;用空格擦除字符
        MOV   AL, Backspace
        CALL  PutChar               ;再次光标回退
        DEC   SI                    ;调整字符串中的存放位置
        DEC   CH                    ;调整字符串中的字符计数
        INC   CL                    ;调整字符串中的空间计数
        JMP   SHORT  .Lab1          ;继续接受字符
        ;
    .Lab5:
        MOV    [SI], AL             ;保存最后的回车符
    .Lab6:
        POP   SI
        RET
    ;-------------------------------
    PutChar:                        ;显示一个字符
        MOV   BH, 0
        MOV   AH, 14
        INT   10H
        RET
    ;
    GetChar:                        ;键盘输入一个字符
        MOV   AH, 0
        INT   16H
        RET
    ;-------------------------------
    PutStr:                         ;显示字符串（以0结尾）
        MOV   BH, 0
        MOV   SI, DX
    .Lab1:
        LODSB
        OR    AL, AL
        JZ    .Lab2
        MOV   AH, 14
        INT   10H
        JMP   .Lab1
    .Lab2:
        RET
    ;-------------------------------
    DiskAP:                         ;磁盘地址包
        DB    10H                   ;DAP尺寸
        DB    0                     ;保留
        DW    1                     ;扇区数
        DW    0                     ;缓冲区偏移
        DW    ZONETEMP              ;缓冲区段值
        DD    0                     ;起始扇区号LBA的低4字节
        DD    0                     ;起始扇区号LBA的高4字节
    ;-------------------------------
    buffer:                         ;缓冲区
        db    9                     ;缓冲区的字符串容量
        db    "123456789"           ;存放字符串
    ;-------------------------------
    mess0     db    "0-halt 1-Snake!!!", 0
    mess1     db    "Invaild code...", 0DH, 0AH, 0
    mess2     db    "Reading disk error...", 0DH, 0AH, 0
    mess3     db    "Halt...", 0
	mess4     db    0DH, 0AH,"error!Please re-enter:", 0
    ;-------------------------------
    times   510 - ($ - $$) db   0   ;填充0，直到510字节
        db    55h, 0aah             ;最后2字节，共计512字节
