# Testing Libopusfile

Please ensure stack is properly installed. Run
```bash
./build.sh
```
This will build the target library and generate three executables: `test`, `debug` and `trace`.
- `test`: conducts fuzzing via libfuzzer.
- `debug`: conducts fuzzing via libfuzzer, prints out function calls.
- `trace`: generates bug-inducing C program from `test`-generated crash files.
