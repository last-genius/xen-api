.section ".note.gnu.build-id", "a"
    .p2align 2
# name size (not including padding)
    .long 1f - 0f
# desc size (not including padding)
    .long 3f - 2f
# type
    .long 0x1
# name
0:  .asciz "gnu.build-id"
1:  .p2align 2
# desc
2:  .long 0x000000
3:  .p2align 2
