include <urmom.s>

Ship struct
	x dword ?
	y dword ?
Ship ends

.data?

ship Ship <>


.code

main proc
	ret
main endp

mov rax, (2 + 4) * 3 shl 3
mov rbx, rax
ret

end
