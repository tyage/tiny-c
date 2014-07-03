GLOBAL	foo
foo:
push	ebp
mov	ebp, esp
sub	esp, 4
mov	eax, [ebp+8]
imul	eax, [ebp+8]
mov	[ebp-4], eax
mov	eax, [ebp-4]
add	eax, 2
mov	esp, ebp
pop	ebp
ret