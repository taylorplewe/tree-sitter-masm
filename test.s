MyStruct struct
	align 1
	urmom byte ?
	align 8
	myvar2 dword ?
MyStruct ends

urmom2 proto
urmom4 typedef dword


mymac macro
	ret
endm


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
end
