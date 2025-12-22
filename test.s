ifndef SHIP_H
SHIP_H = 1

MyStruct struct
	align 1
	urmom byte ?
	align 8
	myvar2 dword ?
    shrink_vec       Vector <?> ; p1 will ADD this value to get towards the center, p2 will SUBTRACT
MyStruct ends

urmom2 proto
urmom4 typedef dword


mymac macro
	ret
endm


.data

fires      Fire  MAX_NUM_FIRES dup (<?>)
fires_arr  Array { { fires, 0 }, MAX_NUM_FIRES, sizeof Fire }
fire_color Pixel <0ffh, 0, 0, 0ffh>

; in:
	; r8  - point1 (as qword ptr)
	; r10 - point2 (as qword ptr)
	; bl  - rotation in 256-based radians

.code

otherproc proc uses rax rbx rdx rcx
	ret
otherproc endp

alias <ben> = <otherproc>

main proc
	call ben
	mov eax, MyStruct.myvar2
	ret
main endp

fire_create proc
	push rsi
	push r9
	push r11
	push r13

	lea rsi, lowword fires_arr
	call array_push
	test eax, eax
	je _end

	mov rsi, rax

	mov [rsi].Fire.rot, bl
fire_create endp


endif
