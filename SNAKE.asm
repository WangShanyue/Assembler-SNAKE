;该程序为简易版贪吃蛇游戏
;使用方法:将其放入任意扇区号中，然后通过主引导程序加载。
    section   text
    bits   16
    ;工作程序特征信息
    Signature     db   "YANG"       ;签名信息
    Version       dw   1            ;格式版本
    Length        dw   end_of_text  ;工作程序长度
    Start         dw   Begin        ;工作程序入口点的偏移
    Zoneseg       dw   0088H        ;工作程序期望的内存区域起始段值
    Reserved      dd   0            ;保留
	
	S_PO  resb 0x80 												;小蛇位置
	PORT_KEY_DAT   EQU   0x60
    PORT_KEY_STA   EQU   0x64
	GO_RIGHT EQU 31H												;小蛇的方向
	GO_LEFT  EQU 32H
	GO_UP    EQU 33H
	GO_DOWN  EQU 34H
    S_LEN dw 5 														;小蛇初始长度
	STATE db GO_UP 													;小蛇行走状态
	SCORE dw 0														;当前的分数	
	SCORE_ADD dw 1													;增加的分数值
	stone_state db 0;												;果子的状态
	stone_position db 0,0											;果子的位置
    wall1 db "###################################################"	;显示墙壁
		  db 0AH,0DH,0
    wall2 db '#' 													;显示墙壁
          db  "                                                 " 
          db '#',0AH,0DH,0	  										
	wel  db "Welcome to play this funny game! -- WSY :)",0			;提示界面欢迎信息
	wel2 db "Press any key to continue:",0							;初始界面提示信息
		 db 0AH,0DH,0
	faliure db "You have failed, TRY again! :)",0					;失败提示信息
		 db 0AH,0DH,0
	scorestr db "score:"											;显示分数
	SCORE_STRING db 30H,0,0,0,0,0									;显示分数
	CurLin db 11													;输出信息的行值
	CurCol db 6                 									;输出信息的列值
	count    DB   1                 								;计数器
    old1ch   DD   0                 								;用于保存原1CH号中断向量
	
	;主程序入口
	Begin:
        MOV   AX, CS
        MOV   DS, AX  
        CLD   
		CALL CLEAR		;清屏
        CALL WELL_SHOW  ;显示墙壁
		CALL WELCOME_SHOW ;显示欢迎界面
	WAIT_KEY:
		MOV   AH, 1
        INT   16H		;等待并接受用户按键
		JZ	WAIT_KEY
		
		CALL CLEAR		;清屏
		CALL WELL_SHOW	;显示墙壁
		CALL INIT		;初始化蛇的坐标
		CALL KEY_BOARD
		CALL SHOW_TIME	
		CALL CLEAR		;清屏
		CALL FALIURE_SHOW
		RETF
;----------------------------------------------------------------------------
;子程序名：ADD_SCORE
;功能：小蛇吃到果子之后增加分数
;入口参数：无
;出口参数：无                    
;-----------------------------------------------------------------------------		
ADD_SCORE:
		PUSH DS
		MOV   AX, CS
        MOV   DS, AX
		MOV CX, [SCORE_ADD]
		ADD [SCORE],CX
		POP DS
		RET 
;----------------------------------------------------------------------------
;子程序名：SHOW_SCORE
;功能：显示分数
;入口参数：无
;出口参数：无                   
;-----------------------------------------------------------------------------	
SHOW_SCORE:
		PUSH DS
		MOV AX,CS
		MOV DS,AX
		MOV BX,  SCORE
		MOV DX,[BX]    
		MOV BX,  SCORE_STRING  
		CALL PRINT_DEC
		MOV AL,[CurCol]
		PUSH AX
		MOV AL,60
		MOV BYTE [CurCol],AL
		MOV AL,[CurLin]
		PUSH AX
		MOV BYTE [CurLin],1
		MOV DX,  scorestr
		CALL PutStr_Red
		POP AX
		MOV [CurLin],AL
		POP AX 
		MOV [CurCol],AL
		POP DS
		RET         
;----------------------------------------------------------------------------
;子程序名：PRINT_DEC
;功能：将二进制转换为十进制
;入口参数：BX，DX
;出口参数：无                  
;-----------------------------------------------------------------------------		
PRINT_DEC:    
        XOR CX,CX
		MOV AX,DX
    PR_LAB1:
		MOV DX,0
		CMP AX,0
		JE	END_DIV
		PUSH SI
		MOV SI,10
		DIV SI
		POP SI	
		PUSH DX
		INC CX
		JMP PR_LAB1
	END_DIV: 
        CMP CX,0
        JE  END_POP
        POP DX
        ADD DL,0X30
        MOV [BX],DL
        INC BX 
        DEC CX
        JMP END_DIV
	END_POP:
		RET  
;----------------------------------------------------------------------------
;子程序名：FALIURE_SHOW
;功能：输出失败界面
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------	
FALIURE_SHOW:
		MOV   AX, CS
        MOV   DS, AX  
		MOV  DX,faliure	
		CALL PutStr_Red
		JMP $
		RET 
;----------------------------------------------------------------------------
;子程序名：JUDGE
;功能：判断是否存活：是否撞墙，是否吃自己
;入口参数：无
;出口参数：bool类型变量来判断蛇是存活                  
;-----------------------------------------------------------------------------		
	JUDGE:
		PUSH DS
		MOV AX,CS
		MOV DS,AX
		MOV SI,  S_PO	;根据位置判断是否撞墙
		MOV AH,[SI]
		MOV AL,[SI+1]
		CMP AH,0
		JLE  FALIURE
		CMP AH,20
		JGE  FALIURE
		CMP AL,0
		JLE  FALIURE
		CMP AL,50
		JGE  FALIURE
		
		MOV BX,  S_LEN
		MOV CX,[BX]   
		DEC CX
	SELF:		;判断是否吃到了自己
		ADD SI,2   
		MOV DH,[SI]
		MOV DL,[SI+1]
		CMP DH,AH
		JNE S_LAB1
		CMP DL,AL
		JNE S_LAB1
		JMP FALIURE
	   S_LAB1:
		LOOP SELF  
		MOV AX,0
		JMP SURVIVE
	  FALIURE:
		MOV AX,1
	  SURVIVE:
		POP DS
		RET
	
;----------------------------------------------------------------------------
;子程序名：CLEAR
;功能：清屏指令
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------
CLEAR:
		MOV AH,0X00                
		MOV AL,0X03
		INT 0X10
		RET 
;----------------------------------------------------------------------------
;子程序名：WELL_SHOW
;功能：显示墙壁
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------
WELL_SHOW:
		MOV   DX,  wall1
        CALL  PutStr                
        ;     
        MOV   DX,  wall2 
        MOV  CX,10
 .LAB1:
        
        CALL PutStr
        LOOP  .LAB1     
		MOV CX,10
		MOV DX,wall2
 .LAB2:
        
        CALL PutStr
        LOOP   .LAB2
		
        MOV   DX,  wall1
        CALL  PutStr
		RET 
;----------------------------------------------------------------------------
;子程序名：WELCOME_SHOW
;功能：显示刚开始的欢迎字符串
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------
WELCOME_SHOW:
		MOV DX,wel
		CALL PutStr_Red
		MOV AL,  [CurCol]
		ADD AL,5
		MOV [CurCol], AL
		MOV AL,  [CurLin]
		ADD AL,2
		MOV [CurLin], AL
		MOV DX,wel2
		CALL PutStr_Red
		RET 
;----------------------------------------------------------------------------
;子程序名：PUTSTR
;功能：显示字符串
;入口参数：DX
;出口参数：无                  
;-----------------------------------------------------------------------------		
 PutStr:                         ;显示字符串（以0结尾）
        MOV   BH, 0
        MOV   SI, DX                ;DX=字符串起始地址偏移
    LAB1:
        LODSB
        OR    AL, AL
        JZ    LAB2
        MOV   AH, 14
        INT   10H
        JMP   LAB1
    LAB2:
        RET
;----------------------------------------------------------------------------
;子程序名：PutStr_Red
;功能：指定位置显示字符串
;入口参数：字符串的地址，存放到dx中
;出口参数：无                  
;-----------------------------------------------------------------------------
PutStr_Red: ;显示字符串（以0结尾）
		PUSH SI
		MOV SI,DX
		MOV DL ,[CurCol]
		MOV AL,[SI]
	Red_Lab1:
		MOV DH,[CurLin]
		MOV BL,0X07
		MOV BH,0
		MOV CX,1
		;
		MOV AH,2
		INT 10H
		;
		MOV AH,9
		INT 10H
		;
		INC DL
		INC BL
		INC SI
		MOV AL,[SI]
		OR AL,AL
		JNZ Red_Lab1
		MOV DH,23
		MOV DL,0
		MOV AH,2
		INT 10H
		
		POP SI
		RET 
;----------------------------------------------------------------------------
;子程序名：ENTRY_1CH
;功能：时钟调用程序
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------		
    Entry_1CH:
        DEC   BYTE  [CS:count]      ;计数器减1
        JZ    ETIME                 ;当计数为0，显示时间
        IRET                        ;否则，中断返回
        ;
    ETIME:                          
        MOV   BYTE [CS:count], 6   ;重新设置计数初值
        ;
        STI                         ;开中断
        PUSHA                       ;保护现场
        CALL  get_time              ;获取当前时间
        CALL  EchoTime              ;显示当前时间
		call  EchoSnake
		call SHOW_SCORE
		call JUDGE
		CMP AX,1
		JNE .LAB1	
		CALL FALIURE_SHOW
		.LAB1:	
        POPA                        ;恢复现场
        IRET                        ;中断返回
    ;------------------------------
    get_time:                       ;简化方式获取实时时钟（时分秒）
        MOV   AL, 4                 ;准备读取时值
        OUT   70H, AL
        IN    AL, 71H               ;获取时值
        MOV   CH, AL                ;CH=时值BCD码
        MOV   AL, 2                 ;准备读取分值
        OUT   70H, AL
        IN    AL, 71H               ;获取分值
        MOV   CL, AL                ;CL=分值BCD码
        MOV   AL, 0                 ;准备读取秒值
        OUT   70H, AL
        IN    AL, 71H               ;获取秒值
        MOV   DH, AL                ;DH=秒值BCD码
        RET
    ;------------------------------
    %define   ROW     0            ;时间显示位置行号
    %define   COLUMN  60            ;时间显示位置列号
    EchoTime:                       ;显示当前时间（时分秒）
        PUSH  SI
        ;-----                      ;设置显示时间的位置
        PUSH  DX                    ;保存入口参数
        PUSH  CX
        MOV   BH, 0
        MOV   AH, 3                 ;取得当前光标位置
        INT   10H
        MOV   SI, DX                ;保存当前光标位置
        MOV   DX, (ROW<<8) + COLUMN
        MOV   AH, 2
        INT   10H                   ;设置光标位置
        POP   CX
        POP   DX
        ;-----                      ;显示当前时间（时:分:秒）
        MOV   AL, CH
        CALL  EchoBCD               ;显示时值
        MOV   AL, ':'
        CALL  PutChar
        MOV   AL, CL
        CALL  EchoBCD               ;显示分值
        MOV   AL, ':'
        CALL  PutChar
        MOV   AL, DH
        CALL  EchoBCD               ;显示秒值
        ;-----                      ;恢复光标原先位置
        MOV   DX, SI
        MOV   AH, 2
        INT   10H
        POP   SI
        RET
    ;------------------------------ 
    EchoBCD:                        ;显示2位BCD码值
        PUSH  AX
        SHR   AL, 4
        ADD   AL, '0'
        CALL  PutChar
        POP   AX
        AND   AL, 0FH
        ADD   AL, '0'
        CALL  PutChar
        RET
    ;------------------------------
    PutChar:                        ;TTY方式显示一个字符
        MOV   BH, 0
        MOV   AH, 14
        INT   10H
        RET
    ;------------------------------
    SHOW_TIME:     
        MOV   AX, CS
        MOV   DS, AX                ;DS = CS
        MOV   SI, 1CH*4             ;1CH号中断向量所在地址
        MOV   AX, 0
        MOV   ES, AX                ;ES = 0
        ;保存1CH号中断向量
        MOV   AX, [ES:SI]
        MOV   [old1ch], AX          ;保存向量之偏移
        MOV   AX, [ES:SI+2]
        MOV   [old1ch+2], AX        ;保存向量之段值
        ;设置新的1CH号中断向量
        CLI                         ;关中断
        MOV   AX, Entry_1CH
        MOV   [ES:SI], AX           ;设置新向量之偏移
        MOV   AX, CS
        MOV   [ES:SI+2], AX         ;设置新向量之段值
        STI                         ;开中断
    Continue:  
		call show_stone
		call JUDGE_stone
		MOV   AH,1
		INT 16H
		JZ	Continue
		
		MOV   AH, 0
        INT   16H		;接受用户按键
        PUSH DS
		PUSH AX
		MOV   AX, CS
		MOV   DS, AX  

		POP AX
        CMP AL,20H;D IS ->
        JE	LAB_RIGHT
		CMP AL,1EH;A IS <-
		JE  LAB_LEFT
		CMP AL,11H
		JE	LAB_UP
		CMP AL,1FH
		JNE NEXT
	LAB_DOWN:
		MOV AL,BYTE [STATE];之前是上则不能按下，下面同理
		CMP AL,GO_UP
		JE NEXT
		MOV BYTE [STATE],GO_DOWN
		JMP NEXT
	LAB_RIGHT:
		MOV AL,BYTE [STATE]
		CMP AL,GO_LEFT
		JE NEXT
		MOV BYTE [STATE],GO_RIGHT
		JMP NEXT
	LAB_UP:
		MOV AL,BYTE [STATE]
		CMP AL,GO_DOWN
		JE NEXT
		MOV BYTE [STATE],GO_UP
		JMP NEXT
	LAB_LEFT:
		MOV AL,BYTE [STATE]
		CMP AL,GO_RIGHT
		JE NEXT
		MOV BYTE [STATE],GO_LEFT
		JMP NEXT
	NEXT:
		call  SNAKE_CLEAR
		call  MOVE
		call  SNAKE_SHOW 
		pop   DS	
        jmp    Continue
		
        MOV   EAX, [CS:old1ch]      ;获取保存的原1CH号中断向量
        MOV   [ES:SI], EAX          ;恢复原1CH号中断向量
        ;
        RET
	;----------------------------------------------------------------------------
;子程序名：EchoSnake
;功能：调用蛇的显示、移动、清空函数来产生蛇移动的动画
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------	
EchoSnake:
		PUSH  SI
		call  SNAKE_CLEAR
		call  MOVE
		call  SNAKE_SHOW 
		;-------------------------
        POP   SI
        RET
;----------------------------------------------------------------------------
;子程序名：KEY_BOARD
;功能：键盘中断处理程序代码
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------		
KEY_BOARD:                          
        MOV   AX, 0                     ;准备设置中断向量
        MOV   DS, AX
        CLI
        MOV   WORD [9*4], int09h_handler
        MOV   [9*4+2], CS               ;启用新的键盘中断处理程序
        STI
        ret                           ;结束（返回到加载器）
;-----------------------------------
    int09h_handler:                     ;新的9号键盘中断处理程序，作用是只接受ASDW四个键
        PUSHA                           ;保护通用寄存器
        ;
        MOV   AL, 0ADH
        OUT   PORT_KEY_STA, AL          ;禁止键盘发送数据到接口
        ;
        IN    AL, PORT_KEY_DAT          ;从键盘接口读取按键扫描码
        ;
        STI                             ;开中断
        CALL  Int09hfun                 ;完成相关功能
        ;
        CLI                             ;关中断
        MOV   AL, 0AEH
        OUT   PORT_KEY_STA, AL          ;允许键盘发送数据到接口
        ;
        MOV   AL, 20H                   ;通知中断控制器8259A
        OUT   20H, AL                   ;当前中断处理已经结束
        ;
        POPA                            ;恢复通用寄存器
        ;
        IRET                            ;中断返回
    ;-----------------------------------
    Int09hfun:                          ;演示9H号中断处理程序的具体功能
    .LAB1:								;仅识别处理WASD十个键
        CMP   AL, 1EH                   ;判断字母A键扫描码
        JB    .LAB4                    ;低于，则直接丢弃
        CMP   AL, 20H                   ;判断字母D键扫描码
        JA    .LAB3                     
	.LAB5:	
        MOV   AH, AL                    ;保存扫描码
    .LAB2:
        CALL  Enqueue   
		jmp .LAB3
	.LAB4:
	    CMP AL,11H
		je .LAB5
    .LAB3:
        RET                          
	
    ;-----------------------------------
    Enqueue:                            ;把扫描码和ASCII码存入键盘缓冲区
		PUSH  DS                        ;保护DS
        MOV   BX, 40H
        MOV   DS, BX                    ;DS=0040H
        MOV   BX, [001CH]               ;取队列的尾指针
        MOV   SI, BX                    ;SI=队列尾指针
        ADD   SI, 2                     ;SI=下一个可能位置
        CMP   SI, 003EH                 ;越出缓冲区界吗？
        JB    .LAB1                     ;没有，转
        MOV   SI, 001EH                 ;是的，循环到缓冲区头部
    .LAB1:
        CMP   SI, [001AH]               ;与队列头指针比较
        JZ    .LAB2                     ;相等表示，队列已经满
        MOV   [BX], AX                  ;把扫描码和ASCII码填入队列
        MOV    [001CH], SI              ;保存队列尾指针
    .LAB2:
        POP   DS                        ;恢复DS
        RET                             ;返回
;----------------------------------------------------------------------------
;子程序名：INIT
;功能：初始化小蛇程序
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------	
INIT:   
		XOR SI,SI
		MOV BX, S_LEN
		MOV CX,[BX] 
		MOV BX, S_PO  
		MOV DH,12
	INIT_LAB1:
		MOV BYTE [BX+SI],12
		MOV BYTE [BX+SI+1],DH
		DEC DH    
		ADD SI,2
		LOOP  INIT_LAB1
		RET   
;----------------------------------------------------------------------------
;子程序名：SNAKE_SHOW
;功能：显示小蛇
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------		
SNAKE_SHOW:
		PUSH DS
		MOV   AX, CS
		MOV   DS, AX  
		XOR SI,SI 
		MOV BX, S_LEN
		MOV CX,[BX] 
		MOV BX, S_PO
	SHOW_LAB1:
		MOV DH,[BX+SI]
		MOV DL,[BX+SI+1]
		MOV AL,'#'
		PUSH BX
		MOV BH,0
		MOV BL,04H
		PUSH CX 
		MOV CX,[SCORE] ;每得4分换一种颜色
		SHR CX,2
		ADD BL,CL
		POP CX
		MOV AH,2
		INT 10H
		PUSH CX
		MOV CX,1
		MOV AH,9
		INT 10H
		POP CX
		POP BX
		ADD SI,2   
		LOOP SHOW_LAB1
		POP DS
		RET    
;----------------------------------------------------------------------------
;子程序名：MOVE
;功能：移动小蛇程序
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------   
MOVE:  
		PUSH DS
		MOV   AX, CS
		MOV   DS, AX  
		MOV BX, S_LEN
		MOV CX,[BX]        
		MOV SI, S_PO  
		MOV DI,SI 
		PUSH SI   ;事先存下数组首地址   
		SHL CX,1;CX*2 是大小
		SUB CX,2;CX-2 是指数组最后一个元素的位置
		ADD DI,CX;DI指向最后一个元素
		SUB CX,2;CX再减2
		ADD SI,CX ;SI存放倒数第二个元素的位置
		ADD CX,2  
		SHR CX,1  ;CX+2再除以2之后得到的是移动的长度，
	CPYMEM:
		MOV AX,[SI]
		MOV [DI],AX
		SUB SI,2
		SUB DI,2
		LOOP CPYMEM 
		POP SI   ;取出事先存下数组首地址    
		MOV BX,  STATE  
		MOV CL,[BX] 
		CMP CL,GO_RIGHT
		JE  TURN_RIGHT
		CMP CL,GO_LEFT
		JE  TURN_LEFT
		CMP CL,GO_UP
		JE  TURN_UP  
		INC BYTE [SI]
		JMP   TURN_END 
		TURN_UP:  
		DEC BYTE [SI] 
		JMP   TURN_END 
		TURN_LEFT:
		DEC BYTE [SI+1]     
		JMP   TURN_END 
		TURN_RIGHT:    
		INC BYTE [SI+1]
		TURN_END:
		POP DS
		RET 	
;----------------------------------------------------------------------------
;子程序名：SNAKE_CLEAR
;功能：擦除小蛇程序
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------	
SNAKE_CLEAR:
		PUSH DS
		MOV   AX, CS
		MOV   DS, AX  
		XOR SI,SI 
		MOV BX, S_LEN
		MOV CX,[BX] 
		MOV BX, S_PO
	.LAB1:
		MOV DH,[BX+SI]
		MOV DL,[BX+SI+1]
		MOV AL,' '
		PUSH BX
		MOV BH,0
		MOV BL,0
		MOV AH,2
		INT 10H
		PUSH CX
		MOV CX,1
		MOV AH,9
		INT 10H
		POP CX
		POP BX
		ADD SI,2   
		LOOP .LAB1
		POP DS
		RET
;----------------------------------------------------------------------------
;子程序名：RAND
;功能：参审随机果子
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------	
RAND:
	  PUSH DS
	  MOV AX,CS
	  MOV DS,AX
      PUSH CX
      PUSH DX
      PUSH AX
      STI
      MOV AH,0             ;读时钟计数器值
      INT 1AH
      MOV AX,DX            ;清高6位
      AND AH,3
      MOV CX,AX
      MOV DL,19           ;除19，产生0~19余数
      DIV DL
      MOV BL,AH            
      INC BL               ;石头的列存入BL
      MOV [stone_position],BL;
      MOV AX,CX;
      MOV DL,49           ;除49，产生0~49余数
      DIV DL
      MOV BL,AH            
      INC BL               ;石头的列存入BL
      MOV [stone_position+1],BL;
      POP AX
      POP DX
      POP CX
	  POP DS
      RET
;----------------------------------------------------------------------------
;子程序名：SHOW_STONE
;功能：显示果子
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------
show_stone:
	  PUSH DS
	  MOV AX,CS
	  MOV DS,AX 
	  PUSH AX
	  PUSH BX
	  PUSH CX
	  PUSH DX
	  MOV AL,[stone_state]
	  CMP AL,0
	  JNE LABLE1
	  CALL RAND
	  MOV AL,1
	  MOV [stone_state],AL;
LABLE1:	  
      MOV BX,stone_position
      MOV DH,[BX]
      MOV DL,[BX+1]
      MOV AL,'$'
      MOV BH,0
      MOV BL,30H
	  PUSH CX 
	  MOV CX,[SCORE] ;每得4分换一种颜色
	  SHR CX,2
	  ADD BL,CL
	  POP CX
      MOV AH,2
      INT 10H;
      MOV CX,1
      MOV AH,9H
      INT 10H
	  POP DX
	  POP CX
	  POP BX
	  POP AX
	  POP DS 
	  RET;
;----------------------------------------------------------------------------
;子程序名：SHOW_STONE
;功能：判断小蛇是否吃到果子
;入口参数：无
;出口参数：无                  
;-----------------------------------------------------------------------------
JUDGE_stone:
		PUSH DS
		MOV AX,CS
		MOV DS,AX
		MOV BX,[S_PO]
		MOV DI,BX;DI存蛇头的位置
		MOV BX,[stone_position]
		MOV SI,BX;SI存石子的位置
		CMP SI,DI
		JNE NOT_EAT_STONE
		INC SI
		INC DI
		CMP SI,DI
		JNE NOT_EAT_STONE;
		MOV BX,[S_LEN]
		INC BX;
		MOV [S_LEN],BX;
		XOR BL,BL;
		MOV [stone_state],BL
		CALL ADD_SCORE
	NOT_EAT_STONE:
		POP DS
		RET
;---------------------
end_of_text:
