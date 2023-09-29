lib_dir = System.get_env("ELIXIR_LIB_DIR", "")
out_dir = System.get_env("ELIXIR_OUT_DIR", "")
files = Path.wildcard("#{lib_dir}/**/*.ex")

File.mkdir_p!(out_dir)

IO.puts("#{IO.ANSI.magenta}===> Compiling elixir files...#{IO.ANSI.reset}")

Kernel.ParallelCompiler.compile_to_path(files, out_dir)
