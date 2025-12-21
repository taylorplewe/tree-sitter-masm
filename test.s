SCREEN_WIDTH = 256
SCREEN_HEIGHT = 224


.data?

pixels db SCREEN_WIDTH * SCREEN_HEIGHT dup (?)


.code

mov rax, 4
add rax, 2 + 4

end
