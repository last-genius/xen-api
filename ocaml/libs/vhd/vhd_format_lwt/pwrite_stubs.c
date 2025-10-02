/*#include "lwt_config.h"*/

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <unistd.h>

CAMLprim value xapi_pwrite_stub(value val_fd, value val_buf, value val_file_ofs, value val_ofs, value val_len)
{
    long ret;
    ret = pwrite(Int_val(val_fd),
                (char *)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs),
                Long_val(val_len), Long_val(val_file_ofs));
    if (ret == -1) uerror("pwrite", Nothing);
    return Val_long(ret);
}
