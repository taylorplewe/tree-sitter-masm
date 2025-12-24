MyStruct struct
	align 1
	urmom byte ?
	align 8
	myvar2 dword ?
MyStruct ends

urmom2 proto

urmom4 typedef dword

UrMom macro
	mov rax, 4
	add rax, 2
endm

.code

otherproc proc uses rax rbx rdx rcx
    mov dil, 2
	ret
otherproc endp

alias <ben> = <otherproc>

mymac macro
	urmanz2 dword ?
	insidemac proc
		mov eax, 1
		ret
	insidemac endp
endm

myproc proc
	labelzz: mov eax, 2
	ret
	mylabel:
myproc endp

main proc
	urmanz dword ?

	mymac

	call ben
	mov eax, MyStruct.myvar2
	ret
main endp
end
