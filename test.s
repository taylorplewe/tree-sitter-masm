include <ship.s>

Pixel struct
	r byte 0
	g byte 0
	b byte 0
	a byte 0
Pixel ends

SCREEN_WIDTH = 256
SCREEN_HEIGHT = 224
SCREEN_AREA = SCREEN_WIDTH * SCREEN_HEIGHT


.data?

pixels Pixel SCREEN_AREA dup(<>)


.code

main proc
	mov rax, (SCREEN_AREA * sizeof Pixel) + 2
	ret
main endp

end
