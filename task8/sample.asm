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
push	0
mov	eax, 1
push	eax
mov	eax, [ebp-4]
pop	ebx
cmp	eax, ebx
sete	al
movzx	eax, al
cmp	eax, 0
je	L0
mov	eax, 2
push	eax
mov	eax, [ebp+8]
pop	ebx
cmp	eax, ebx
setg	al
movzx	eax, al
cmp	eax, 0
je	L0
pop	eax
push	1
L0:
pop	eax
cmp	eax, 1
je	L1
jmp	L2
L1:
mov	eax, 1
push	eax
mov	eax, [ebp-4]
pop	ebx
add	eax, ebx
mov	[ebp-4], eax
L2:
L3:
mov	eax, 4
push	eax
mov	eax, [ebp-4]
pop	ebx
cmp	eax, ebx
setl	al
movzx	eax, al
cmp	eax, 0
je	L4
mov	eax, 1
push	eax
mov	eax, [ebp-4]
pop	ebx
add	eax, ebx
mov	[ebp-4], eax
jmp	L3
L4:
mov	eax, 2
push	eax
mov	eax, [ebp-4]
pop	ebx
add	eax, ebx
mov	esp, ebp
pop	ebp
ret
L5:
mov	esp, ebp
pop	ebp
ret