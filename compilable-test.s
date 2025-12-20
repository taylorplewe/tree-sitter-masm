MyStruct struct
	align 1
	urmom byte ?
	align 8
	myvar2 dword ?
MyStruct ends

UrMom macro
	mov rax, 4
	add rax, 2
endm

.code

otherproc proc
	ret
otherproc endp

alias <ben> = <otherproc>

main proc
	call ben
	mov eax, MyStruct.myvar2
	ret
main endp
end
