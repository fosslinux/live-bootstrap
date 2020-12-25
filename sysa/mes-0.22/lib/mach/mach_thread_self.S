#if 1	
#include <mach/machine/syscall_sw.h>
kernel_trap (__mach_thread_self,-27,0)
// weak_alias (__mach_thread_self, mach_thread_self)
#else
        .globl __mach_thread_self
        .type __mach_thread_self,@function
        .align 1<<4
__mach_thread_self: .cfi_startproc
        movl $ -27,%eax
        .byte 0x9a
        .long 0
        .word 0x7
        ret
        .cfi_endproc
        .size __mach_thread_self,.-__mach_thread_self
        .weak mach_thread_self 
        mach_thread_self = __mach_thread_self
#endif
