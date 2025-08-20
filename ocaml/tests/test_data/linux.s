.section ".note.Linux", "a"
    .p2align 2
# name size (not including padding)
    .long 1f - 0f
# desc size (not including padding)
    .long 3f - 2f
# type
    .long 0x257
# name
0:  .asciz "Linux"
1:  .p2align 2
# desc
2:  .asciz "4.19.0+1"
3:  .p2align 2
