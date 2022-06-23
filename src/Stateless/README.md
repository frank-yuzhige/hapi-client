# Testing Stateless Arithmetic Library

Please ensure stack is properly installed. Run
```bash
./build.sh
```
This will build the target library and generate three executables: `qc`, `test` and `trace`.
- `qc`: conducts fuzzing via quickcheck.
- `test`: conducts fuzzing via libfuzzer.
- `trace`: generates bug-inducing C program from `test`-generated crash files.
