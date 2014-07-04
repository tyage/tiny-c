GLOBAL	_foo
_foo:
push	ebp
mov	ebp, esp
sub	esp, 128
mov	eax, [ebp+8]
push	eax
mov	eax, [ebp+8]
pop	ebx
imul	eax, ebx
mov	[ebp-4], eax
mov	eax, 2
push	eax
mov	eax, [ebp-4]
pop	ebx
add	eax, ebx
mov	esp, ebp
pop	ebp
ret
fooret:
mov	esp, ebp
pop	ebp
ret