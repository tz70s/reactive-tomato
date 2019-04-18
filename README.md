# reactive-tomato

[![Build Status](https://travis-ci.com/tz70s/reactive-tomato.svg?token=q2MTgdyCTSXkarGyJWZp&branch=master)](https://travis-ci.com/tz70s/reactive-tomato)

Reactive `ListT`.

Encode `ListT` as `Signal`, lets you compose events from callbacks as well as working in concurrent and distributed environment.

## Development

Note that **redis** is required to be launched, and the test setting is target to "127.0.0.1:6379" as default.

### Run Examples

Build and executing them.

```bash
stack build

# Native is the one without reactive abstraction.
stack exec colocation-native

# Or, the reactive one.
stack exec colocation-reactive.
```

### Run Tests

To run all unit tests and inspecting cross projects output.

```bash
stack test --interleaved-output

# The specific tests can be run with test arguments (see tasty doc).
# There's also provide scripts for simplicity.
# For example, this will test all tests under Remote.
./test_specific.sh Remote
```
