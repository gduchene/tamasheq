/*
Copyright (c) 2014, Grégoire Duchêne <gduchene@awhk.org>

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*/

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <string.h>

typedef value (*caml_1)(value);
typedef value (*caml_2)(value, value);
typedef value (*caml_3)(value, value, value);
typedef value (*caml_4)(value, value, value, value);
typedef value (*caml_5)(value, value, value, value, value);

extern void	caml_sys_init(char *, char **);

CAMLprim value
tamasheq_call_1(value fname, value arg)
{
        caml_1	f;

        if ((f = dlsym(RTLD_DEFAULT, String_val(fname))) == NULL)
                caml_failwith(dlerror());

        return f(arg);
}

CAMLprim value
tamasheq_call_2(value fname, value arg1, value arg2)
{
        caml_2	f;

        if ((f = dlsym(RTLD_DEFAULT, String_val(fname))) == NULL)
                caml_failwith(dlerror());

        return f(arg1, arg2);
}

CAMLprim value
tamasheq_call_3(value fname, value arg1, value arg2, value arg3)
{
        caml_3	f;

        if ((f = dlsym(RTLD_DEFAULT, String_val(fname))) == NULL)
                caml_failwith(dlerror());

        return f(arg1, arg2, arg3);
}

CAMLprim value
tamasheq_call_4(value fname, value arg1, value arg2, value arg3, value arg4)
{
        caml_4	f;

        if ((f = dlsym(RTLD_DEFAULT, String_val(fname))) == NULL)
                caml_failwith(dlerror());

        return f(arg1, arg2, arg3, arg4);
}

CAMLprim value
tamasheq_call_5(value *args, int argc)
{
        caml_5	f;

        if (argc != 6)
                caml_failwith("unhandled arity");

        if ((f = dlsym(RTLD_DEFAULT, String_val(args[0]))) == NULL)
                caml_failwith(dlerror());

        return f(args[1], args[2], args[3], args[4], args[5]);
}

CAMLprim value
tamasheq_sys_init(value exe_name, value argv, value sz)
{
        static char	 *tamasheq_exe_name;
        static char	**tamasheq_argv;

        size_t i, rsz = Int_val(sz);

        if ((tamasheq_argv = calloc(rsz + 1, sizeof(*tamasheq_argv))) == NULL)
                caml_failwith("not enough memory");

        tamasheq_exe_name = strdup(String_val(exe_name));

        for (i = 0; i < rsz; ++i)
                tamasheq_argv[i] = strdup(String_val(Field(argv, i)));

        caml_sys_init(tamasheq_exe_name, tamasheq_argv);
        return Val_unit;
}
