.section ".note.XenServer", "a"
    .p2align 2
# name size (not including padding)
    .long 1f - 0f
# desc size (not including padding)
    .long 3f - 2f
# type
    .long 0x1
# name
0:  .asciz "XenServer"
1:  .p2align 2
# desc
2:  .asciz "v2.1.3+0.1fix"
3:  .p2align 2
