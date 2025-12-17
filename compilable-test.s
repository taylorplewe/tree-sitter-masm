mystrct struct
	numapples dword ?
mystrct ends


.data?

mys mystrct <>


.code

urmom proto urman:byte,
			urman2: dword,
			:qword,
			cheese: sword,
			:vararg

urmom2 proto

comment &
yaboiiiiii aofjiei owief oifowefj
aoweifaowf owaf oafow
anythingggggg
; this is a comment within a coment
&

main proc
	mov eax, [mys] . numapples
	ret
main endp

end
