; ==========================================
; pmtest8.asm
; 编译方法：nasm pmtest8.asm -o pmtest8.com
; ==========================================

%include	"pm.inc"	; 常量, 宏, 以及一些说明

FreeBlockBase	equ	200000h	; 页目录开始地址:	2M
BitmapBase		equ	201000h
PageDirBase		equ 202000h
PageTblBase		equ	203000h	; 页表开始地址:		2M +  4K

org	0100h
	jmp	LABEL_BEGIN

[SECTION .gdt]
; GDT
;                           段基址,       段界限, 属性
LABEL_GDT:          Descriptor 0,              0, 0                      ; 空描述符
LABEL_DESC_NORMAL:  Descriptor 0,         0ffffh, DA_DRW                 ; Normal 描述符
LABEL_DESC_FLAT_RW: Descriptor 0,        0fffffh, DA_DRW|DA_LIMIT_4K     ; 0~4G
LABEL_DESC_CODE32:  Descriptor 0, SegCode32Len-1, DA_CR|DA_32            ; 非一致代码段, 32
LABEL_DESC_CODE16:  Descriptor 0,         0ffffh, DA_C                   ; 非一致代码段, 16
LABEL_DESC_DATA:    Descriptor 0,      DataLen-1, DA_DRW                 ; Data
LABEL_DESC_STACK:   Descriptor 0,     TopOfStack, DA_DRWA|DA_32          ; Stack, 32 位
LABEL_DESC_VIDEO:   Descriptor 0B8000h,   0ffffh, DA_DRW                 ; 显存首地址
; GDT 结束

GdtLen		equ	$ - LABEL_GDT	; GDT长度
GdtPtr		dw	GdtLen - 1	; GDT界限
		dd	0		; GDT基地址

; GDT 选择子
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT
; END of [SECTION .gdt]

[SECTION .data1]	 ; 数据段
ALIGN	32
[BITS	32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串
_szRAMSize			db	"RAM size:", 0
_szPageErr			db "PA Failure", 0
_szReturn			db	0Ah, 0
; 变量
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_PageTableNumber		dd	0
_MemChkBuf:	times	256	db	0

; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
PageTableNumber		equ	_PageTableNumber- $$
PageErr			equ _szPageErr - $$
DataLen			equ	$ - LABEL_DATA
; END of [SECTION .data1]


; 全局堆栈段
[SECTION .gs]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0

TopOfStack	equ	$ - LABEL_STACK - 1

; END of [SECTION .gs]


[SECTION .s16]
[BITS	16]
LABEL_BEGIN:
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h

	mov	[LABEL_GO_BACK_TO_REAL+3], ax
	mov	[_wSPValueInRealMode], sp

	; 得到内存数
	mov	ebx, 0
	mov	di, _MemChkBuf
.loop:
	mov	eax, 0E820h
	mov	ecx, 20
	mov	edx, 0534D4150h
	int	15h
	jc	LABEL_MEM_CHK_FAIL
	add	di, 20
	inc	dword [_dwMCRNumber]
	cmp	ebx, 0
	jne	.loop
	jmp	LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:

	; 初始化 16 位代码段描述符
	mov	ax, cs
	movzx	eax, ax
	shl	eax, 4
	add	eax, LABEL_SEG_CODE16
	mov	word [LABEL_DESC_CODE16 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE16 + 4], al
	mov	byte [LABEL_DESC_CODE16 + 7], ah

	; 初始化 32 位代码段描述符
	xor	eax, eax
	mov	ax, cs
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah

	; 初始化数据段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	; 初始化堆栈段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK
	mov	word [LABEL_DESC_STACK + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah

	; 为加载 GDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt 基地址
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt 基地址

	; 加载 GDTR
	lgdt	[GdtPtr]

	; 关中断
	cli

	; 打开地址线A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 Code32Selector:0  处

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax

	mov	sp, [_wSPValueInRealMode]

	in	al, 92h		; ┓
	and	al, 11111101b	; ┣ 关闭 A20 地址线
	out	92h, al		; ┛

	sti			; 开中断

	mov	ax, 4c00h	; ┓
	int	21h		; ┛回到 DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段选择子

	mov	ax, SelectorStack
	mov	ss, ax			; 堆栈段选择子

	mov	esp, TopOfStack


	; 下面显示一个字符串
	push	szPMMessage
	call	DispStr
	add	esp, 4

	push	szMemChkTitle
	call	DispStr
	add	esp, 4

	call	DispMemSize		; 显示内存信息

	call	AllocPageDemo

	; 到此停止
	jmp	SelectorCode16:0

AllocPageDemo:
	call SetupPaging
	push 4
	call alloc_pages
	add	esp, 4
	ret

GetPA:
	push	ebp
	mov		ebp, esp
	push	dword [ebp + 8]
	call	getPTEAddr
	add		esp, 4
	mov 	ecx, [eax]
	and 	cx, 0F000H
	mov 	eax, [ebp + 8]
	and 	eax, 0FFFH
	add 	eax, ecx
	leave

getPTEAddr:
	push	ebp
	mov		ebp, esp
	push 	ebx
	push 	edi
	push 	esi
	mov 	ax, SelectorFlatRW
	mov 	ds, ax
	mov 	eax, [ebp + 8]
	shr 	eax, 22
	mov 	ebx, 4
	mul 	ebx				; LA[22:31] * 4
	mov 	ecx, cr3
	add 	eax, ecx		; PDEAddr = PageDirBase + LA[22:31] * 4
	mov 	ecx, [eax]		; PDE = PageTblAddr | Control Bits
	and 	cl, 1
	jz 		.err
	and		cx, 0F000H
	mov		eax, [ebp + 8]
	shr		eax, 12
	and 	eax, 03ffH
	mov 	ebx, 4
	mul 	ebx				; LA[12:21] * 4
	add 	eax, ecx
	jmp		.end
.err:	
	mov 	ebx, 1
.end:	
	pop 	esi
	pop 	edi
	pop 	ebx
	leave
	ret

alloc_pages:
	push 	ebp
	mov 	ebp, esp
	sub 	esp, 8
	push	ebx
	push	edi
	push	esi
	mov		dword [ebp - 4], 0
	mov		dword [ebp - 8], 0
	mov 	ax, SelectorFlatRW
	mov 	es, ax
	mov 	ds, ax
	mov 	esi, FreeBlockBase
	cld
	mov 	ebx, [ebp + 8]

.1:	lodsd						; addr
	mov		[ebp - 4], eax		; ebp-4存放空闲块基址
	test 	eax, eax
	jz 		.err				; 0表示遍历结束
	lodsd						; length
	mov		[ebp - 8], eax		; ebp-8存放空闲块大小
	cmp		eax, ebx			; 空闲块大小满足条件
	jb		.1
	sub 	eax, ebx			; ebx是输入参数：分配页的数目
	mov 	[esi - 4], eax		; 更新空闲块大小
	mov		eax, ebx
	mov		ebx, 4096
	mul		ebx
	add		eax, dword [ebp - 4]; base_new = base_old + #page_alloc * 4096
	mov		[esi - 8], eax 		; 更新空闲块基址

	mov		ax, SelectorData
	mov		ds, ax
	mov 	eax, [PageTableNumber]	; PageTableNumber储存在DS段
	add 	eax, 3 + 512			; eax存放从第n个物理页可以开始分配
	mov 	esi, BitmapBase			; edi存放Bitmap基址
	mov		ebx, eax
	mov		ax, SelectorFlatRW
	mov		ds, ax
	mov		eax, ebx
	mov		ecx, [ebp + 8]
	mov		edi, [ebp - 4]			; edi存放当前分配逻辑地址
.l1:
	xchg	bx, bx
	bts		[esi], eax
	inc 	eax
	jc		.l1
	push	eax
	mov 	ebx, 4096
	mul		ebx				
	mov		ebx, eax	; 找到的物理页地址 = eax * 4096
	sub		ebx, 4096
	add		ebx, PG_P | PG_USU | PG_RWW
	pop		eax

	push	eax
	push	ecx			; 调用者保护寄存器
	push	edi

	call	getPTEAddr
	add		esp, 4
	add		edi, 4096

	mov		[eax], ebx
	pop		ecx
	pop		eax

	loop	.l1	
	jmp		.end
.err:
	xor 	eax, eax
.end:
	pop		esi
	pop		edi
	pop		ebx
	leave
	ret

free_pages:
	push 	ebp
	mov		ebp, esp
	push	ebx
	push	edi
	push	esi
	mov		ax, SelectorFlatRW
	mov		ds, ax
	mov		edi, [ebp + 8]
	mov		ecx, [ebp + 16]
	mov		esi, BitmapBase
.l1:
	push	eax
	push	ecx
	push	edi
	call	GetPA
	add		esp, 4
	add		edi, 4096
	mov		ebx, 4096
	div		ebx
	btr		[esi], eax
	pop		ecx
	pop		eax
	loop	.l1
	pop		esi
	pop		edi
	pop		ebx
	leave
	ret

; 启动分页机制 --------------------------------------------------------------
SetupPaging:
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax	; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz	.no_remainder
	inc	ecx		; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	; 暂存页表个数

	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.

	; 首先初始化页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase	; 此段首地址为 PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	mov ecx, PageTblBase			; 初始化free block table
	mov eax, [PageTableNumber]
	mov ebx, 4096
	mul ebx
	add eax, ecx
	mov edi, FreeBlockBase
	stosd
	mov ecx, [PageTableNumber]
	add ecx, 3 + 512					;占用页数目=PTE+PDE+2M/4K
	mov	eax, 1024 * 1024				; 0-4G空间一共1M个页
	sub eax, ecx						; 剩余逻辑地址页数目=1M-已使用个数
	stosd
	xor eax, eax
	stosd

	mov eax, ecx
	mov ebx, 8
	div ebx
	mov ecx, eax			;ecx是bitmap 0xff字节的个数
	mov edi, BitmapBase
	mov al,	0xff
.fill_bitmap:
	stosb	
	loop .fill_bitmap
	
	mov ecx, edx
	mov eax, 1
	shl eax, cl
	sub eax, 1
	stosb

	mov	eax, PageDirBase
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h
	mov	cr0, eax
	jmp	short .3
.3:
	nop

	ret
; 分页机制启动完毕 ----------------------------------------------------------

; 显示内存信息 --------------------------------------------------------------
DispMemSize:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf
	mov	ecx, [dwMCRNumber]	;for(int i=0;i<[MCRNumber];i++) // 每次得到一个ARDS(Address Range Descriptor Structure)结构
.loop:					;{
	mov	edx, 5			;	for(int j=0;j<5;j++)	// 每次得到一个ARDS中的成员，共5个成员
	mov	edi, ARDStruct		;	{			// 依次显示：BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
.1:					;
	push	dword [esi]		;
	call	DispInt			;		DispInt(MemChkBuf[j*4]); // 显示一个成员
	pop	eax			;
	stosd				;		ARDStruct[j*4] = MemChkBuf[j*4];
	add	esi, 4			;
	dec	edx			;
	cmp	edx, 0			;
	jnz	.1			;	}
	call	DispReturn		;	printf("\n");
	cmp	dword [dwType], 1	;	if(Type == AddressRangeMemory) // AddressRangeMemory : 1, AddressRangeReserved : 2
	jne	.2			;	{
	mov	eax, [dwBaseAddrLow]	;
	add	eax, [dwLengthLow]	;
	cmp	eax, [dwMemSize]	;		if(BaseAddrLow + LengthLow > MemSize)
	jb	.2			;
	mov	[dwMemSize], eax	;			MemSize = BaseAddrLow + LengthLow;
.2:					;	}
	loop	.loop			;}
					;
	call	DispReturn		;printf("\n");
	push	szRAMSize		;
	call	DispStr			;printf("RAM size:");
	add	esp, 4			;
					;
	push	dword [dwMemSize]	;
	call	DispInt			;DispInt(MemSize);
	add	esp, 4			;

	pop	ecx
	pop	edi
	pop	esi
	ret
; ---------------------------------------------------------------------------

%include	"lib.inc"	; 库函数

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段. 由 32 位代码段跳入, 跳出后到实模式
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov	ax, SelectorNormal
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax

	mov	eax, cr0
	and     eax, 7FFFFFFEh          ; PE=0, PG=0
	mov	cr0, eax

LABEL_GO_BACK_TO_REAL:
	jmp	0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
