.section ".note.XenServerTwo", "a"
    .p2align 2
# name size (not including padding)
    .long 1f - 0f
# desc size (not including padding)
    .long 3f - 2f
# type
    .long 0x2
# name
0:  .asciz "XenServer"
1:  .p2align 2
# desc
2:  .asciz "Built on December 25th"
3:  .p2align 2

.section ".note.XenServerTwo", "a"
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
2:  .asciz "2.0.0-rc.2"
3:  .p2align 2

