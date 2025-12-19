MyStruct struct
	align 1
	urmom byte ?
	align 8
	myvar2 dword ?
MyStruct ends

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
